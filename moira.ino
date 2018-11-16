/*---------------------------------------------------------------------------
moira.ino
by Jason Tost

Copyright 2018 by Jason Tost.  All rights reserved.  Please contact me if you
wish to use or modify this code by email at:

linuxaikido@gmail.com



Code for Felicia's Moira costume for Blizzcon 2018.  Targets the Arduino
platform, originally running on an Uno, later moved to a Mega 2560.  Uses
Neopixels for lighting, and tactile, momentary switches on a custom board for
inputs.

All inputs are simply momentary switches, that pull an input pin to ground.
The application relies on the internal pull-up resistors for the Arduino pins.
No external debounce or filtering is present on the switches; debouncing is
completely done in code.  See the ButtonCheck function for the implementation.



History:

2018-11-01 Jason Tost
Initial development


-----------------------------------------------------------------------------*/
#include <Adafruit_NeoPixel.h>


/*---------------------------------------------------------------------------*/
//
// Declarations
//
/*---------------------------------------------------------------------------*/

// Pixel data mode and rate for the Adafruit_NeoPixel library.
#define PIXEL_MODE          NEO_GRB + NEO_KHZ800

// Indicates the IO pin the backpack is connected to on the IO header.
#define PIXEL_PIN_BACKPACK  3

// Indicates the IO pin the rest of the suit is connected to on the IO header.
#define PIXEL_PIN_SUIT      2

// Indicates the brightness of the costume when in floor mode.  The sealed
// lead acid battery used (Duracell SLAA12-1.3F) was capable of holding up
// the entire costume at full brightness for about 12 minutes.  So two
// brightness modes are available.  The costume can run at full brightness to
// show up in stage lights during exhibition.  But when walking around the
// floor, the costume is operated in a dimmer mode.  After modeling the discharge
// curve of this battery, running the costume at 26% brightness would allow the
// costume to operate continuously for 60 to 62 minutes.  This was verified then
// experimentally.  So when running in dim mode, the brightness scalar here
// is applied to pixel color RGB values.
#define BRIGHTNESS          0.26

// NOTE ON REDRAW_PERIOD: Originally as a fault tolerance mechanism I intended
// to have the costume refresh all pixels automatically every REDRAW_PERIOD
// milliseconds.  This would provide a sort of fault tolerance if we developed
// intermittent connection problems between light segments in the costume while
// on the floor.  Stability problems began occurring due to wiring distances
// and power draw, that resulted in occassional data corruption if the pixels
// were written too frequently, without decoupling throughout the costume having
// enough time to recover given wiring inductance and resistance.  This was
// ultimately pushed out to a significantly long period as to never really
// allow these periodic updates, which helps mask some of these periodic update
// problems.
#define REDRAW_PERIOD       1000000

// Period that the buttons are allowed to settle before looking at steady state
// data from pin sampling.
#define BUTTON_PERIOD       50

// Period between changes to phase shift for "chasing" color effects.
#define ULTIMATE_PERIOD     100
#define PRIDE_PERIOD        100

// Base, full-brightness color values for damage and healing.
#define COLOR_DAMAGE        0x0000b0de
#define COLOR_HEALING       0x0076b100

// Indicates the pin used for heartbeat/activity indicator on the arduino.
#define PIN_HEARTBEAT       13

// Define input pins for switches by designator on the custom PCB for the
// hande piece.  These are connected via a piece of CAT5a cable strung up
// the costume arm on the left side.
#define PIN_SW1             22    // org/wht
#define PIN_SW3             24    // org
#define PIN_SW5             26    // blu/wht
#define PIN_SW6             28    // blu
#define PIN_SW2             30    // brn/wht
#define PIN_SW4             32    // brn

// Use aliases to map specific pin functions to switch designators from the
// custom PCB.
#define TOGGLE_BUTTON       PIN_SW1 // Damage <==> healing toggle
#define ULT_BUTTON          PIN_SW3 // Ultimate activation and deactivation
#define BRIGHT_BUTTON       PIN_SW5 // Bright <==> dim toggle
#define PRIDE_BUTTON        PIN_SW6 // Pride mode activation and deactivation
#define IRISH_BUTTON        PIN_SW2 // Irish flag mode activation and deactivation


/*---------------------------------------------------------------------------*/
//
// Data types
//
/*---------------------------------------------------------------------------*/

// Enumerate the states (draw modes) of the costume.
typedef enum _state_e {
  STATE_NONE = 0, // Startup, allows initialization of internal data
  STATE_DAMAGE,   // Damage mode, steady state
  STATE_HEALING,  // Healing mode, steady state
  STATE_ULTIMATE, // Ultimate mode - not defined yet
  STATE_PRIDE,    // Pride mode - rainbows!
  STATE_IRISH     // Irish flag mode
} mstate;

// Logical color attributes for pixels
typedef struct _color_t {
  byte r;   // red
  byte g;   // green
  byte b;   // blue
} color;

// Represents an instance of the state machine data for a single input
// button.  Each button has its own copy.
typedef struct _button_t {
  uint32_t tref;  // Time reference
  byte pin;       // IO pin number
  byte state;     // State variable for debouncing
  bool prev;      // Previous pressed status
  bool pressed;   // Current pressed status
  bool changed;   // Has the pressed value changed?
} button;

// Represents a segment of an Adafruit_NeoPixel stream sequence of pixels.
typedef struct _segment_t {
  Adafruit_NeoPixel* ios; // Pointer to stream object
  int start;      // Starting index within the stream
  int count;      // Length of the segment
  bool fixed;     // Is this a fixed color LED
  color color;    // What is the fixed color?
} segment;


/*---------------------------------------------------------------------------*/
//
// Global application data
//
/*---------------------------------------------------------------------------*/

button TogBtn;    // Toggles between damage in healing in normal mode
button UltBtn;    // Toggles into and out of ultimate mode
button BrightBtn; // Toggles between bright and dim mode for stage or floor
button PrideBtn;  // Easter egg 1--pride mode
button IrishBtn;  // Easter egg 2--irish flag mode

// There are enough pixels that I wanted to break up updates to the series so
// that timing functions are not too badly mangled.  Continuous updates of all
// pixels every time the timing protocol for the pixels allows causes the
// system to spend all its time in a critical section and timing routines normal
// longer represent any approximation of elapsed time.  Series will be updated
// in two pieces, and only when an event in the state machine requires it.
Adafruit_NeoPixel Backpack;
Adafruit_NeoPixel Suit;

int AppState;  // Main application state machine variable
bool Redraw;   // Indicates a redraw is requested by the state machine
bool Sending1; // Indicates a write of the first sequence is to occur
bool Sending2; // Indicates a write of the second sequence is to occur
bool Bright;   // Indicates bright or dim mode

// The Segments array is our pixel map.  I am treating an Adafruit_NeoPixel
// object like a stream.  Each segment corresponds to a sequence of LEDs within
// that stream, which can have color attributes updated arbitrarily by the
// state machine between write sequences.  Code will make changes to the pixel
// map, and then signal that a write should occur with the Redraw flag.  If a
// segment is flagged as fixed, then the color will remain constant regardless
// of effects that are active.  The color field indicates the fixed color to
// draw the pixel in.  If a segment is not static then the color field should
// be ignored in the state machine.
const segment Segments[] = {
//  ios         start   count   fixed 
  { &Backpack,  0,      48,     false,    { 0x00, 0x00, 0x00 } },  // Tubes, canister
  { &Backpack,  48,     1,      false,    { 0xff, 0x00, 0x00 } },  // Lower red on backpack
  { &Backpack,  49,     8,      false,    { 0x00, 0x00, 0x00 } },  // Cylinders
  { &Backpack,  57,     2,      false,    { 0xff, 0x00, 0x00 } },  // Upper red light
  { &Backpack,  59,     4,      false,    { 0x00, 0x00, 0x00 } },  // Needles

  { &Suit,      0,      22,     false,    { 0x00, 0x00, 0x00 } },  // Head piece (color change)
  { &Suit,      22,     2,      false,    { 0xff, 0x00, 0x00 } },  // Head piece (red fixed)
  { &Suit,      24,     4,      false,    { 0x00, 0x00, 0x00 } },  // Shoulder 1, first needle
  { &Suit,      28,     2,      false,    { 0xff, 0x00, 0x00 } },  // Shoulder 1, fixed red
  { &Suit,      30,     8,      false,    { 0x00, 0x00, 0x00 } },  // Shoulder 1, second needle and dangly needle
  { &Suit,      38,     1,      true,     { 0xff, 0x00, 0x00 } },  // Shoulder 1 dangly fixed red
  
  { &Suit,      39,     2,      false,    { 0x00, 0x00, 0x00 } },  // Sleeve 1
  { &Suit,      41,     8,      false,    { 0x00, 0x00, 0x00 } },  // Shoulder 2 needles
  { &Suit,      49,     2,      true,     { 0xff, 0x00, 0x00 } },  // Shoulder 2, fixed red
  { &Suit,      51,     8,      false,    { 0x00, 0x00, 0x00 } },  // Shoulder 2 second needle and dangly needle
  { &Suit,      59,     1,      true,     { 0xff, 0x00, 0x00 } },  // Shoulder 2 dangly fixed red
  { &Suit,      60,     2,      false,    { 0x00, 0x00, 0x00 } },  // Sleeve 2

  { &Suit,      62,     2,      false,    { 0x00, 0x00, 0x00 } },  // Chest side 1 color change
  { &Suit,      64,     2,      false,    { 0x00, 0x00, 0x00 } },  // Chest side 2 color change
  { &Suit,      66,     1,      true,     { 0xff, 0x00, 0x00 } },  // Chest fixed red
  { &Suit,      67,     2,      false,    { 0x00, 0x00, 0x00 } },  // 2 belly
  { &Suit,      69,     8,      false,    { 0x00, 0x00, 0x00 } },  // hip color change
  { &Suit,      77,     6,      true,     { 0xff, 0x00, 0x00 } },  // Thigh red fixed
  { &Suit,      83,     11,     false,    { 0x00, 0x00, 0x00 } },  // Legs and feet (color change)

  { &Suit,      94,     4,      true,     { 0xb1, 0x76, 0x00 } },  // Healing fixed part
  { &Suit,      98,     2,      true,     { 0xff, 0x00, 0x00 } },  // Fixed red on top
  { &Suit,      100,    1,      true,     { 0xb1, 0x76, 0x00 } },  // Fixed healing in hand

  { &Suit,      101,    8,      true,     { 0xb0, 0x00, 0xde } },  // Damage color two strips
  { &Suit,      109,    2,      true,     { 0xff, 0x00, 0x00 } },  // Fixed red
  { &Suit,      111,    3,      true,     { 0x00, 0xb0, 0xde } },  // Fixed damage 
  
  { 0,          0,      0,      false,    { 0x00, 0x00, 0x00 } }   // Last entry in the table
};

// Represents the rainbow animation steps effectively as a piecewise, periodic
// function.  This table represents a single period.  When in pride mode, the
// state machine will handle writing color attributes to specific pixels in
// sequence, adding a time-dependent phase shift so that the colors chase down
// the series.
const color RainbowTab[] = {
  { 0xff, 0x00, 0x00 }, // Red
  { 0xff, 0x7f, 0x00 }, // Orange
  { 0xff, 0xff, 0x00 }, // Yellow
  { 0x00, 0xff, 0x00 }, // Green
  { 0x00, 0x00, 0xff }, // Blue
  { 0x4b, 0x00, 0x82 }, // Indigo
  { 0x94, 0x00, 0x3d }  // Violet
};

// Full-intensity colors for normal healing and damage.  Colors are based on
// Felicia color matching to colors I programmed into the pixels.
const color NormalTab[] = {
  { 0xb0, 0x00, 0xde }, // Damage normal
  { 0xb1, 0x76, 0x00 }, // Healing normal
};

// Ultimate mode piecewise, periodic function.  This approximates a sinusoid
// with an offset, with half alternations being in damage or healing, with
// an approximate sinusoidal fade between them.  As with pride mode, the
// chasing effect comes from the state handler introducing a time-dependent
// phase shift into pixels in the sequence being assigned colors from this
// function.
const color UltTab[] = {
  { 0x13, 0x00, 0x18 }, // Off
  { 0x3a, 0x00, 0x4a }, // Third damage
  { 0xb0, 0x00, 0xde }, // Full damage
  { 0x3a, 0x00, 0x4a }, // Third damage
  { 0x13, 0x0d, 0x00 }, // Off
  { 0x3a, 0x27, 0x00 }, // Third healing
  { 0xb1, 0x76, 0x00 }, // Full healing
  { 0x3a, 0x27, 0x00 }, // Third healing
};

// Homage to Moira's backstory; she is supposed to be from Dublin, so an
// easter egg mode will be to color the costume in the colors of the Irish
// flag.  This mode just segments out even thirds of the LEDs to each color.
const color IrelandTab[] = {
  { 0x16, 0x9b, 0x62 }, // Green
  { 0xff, 0xff, 0xff }, // White
  { 0xff, 0x88, 0x3e }  // Orange
};


/*---------------------------------------------------------------------------*/
//
// Implementations of utility functions
//
/*---------------------------------------------------------------------------*/

#define BSTATE_UNPRESSED  0 // Button is not pressed.
#define BSTATE_DEBOUNCE   1 // Button has been pressed, waiting for bounces to die down.
#define BSTATE_PRESSED    2 // Button is considered pressed.

/*---------------------------------------------------------------------------
Function: ButtonInit
Arguments:
  b   - Pointer to a button to initialize.
  pin - Input pin number to utilize for the button.
Returns: nothing

Initialize the button's state machine and input pin appropriately for use
with a single input signal.
---------------------------------------------------------------------------*/
void ButtonInit(button* b, byte pin)
{
  b->tref = millis();
  b->pin = pin;
  b->state = BSTATE_UNPRESSED;
  b->prev = false;
  b->pressed = false;
  b->changed = false;

  pinMode(pin, INPUT_PULLUP);
}

/*---------------------------------------------------------------------------
Function: ButtonCheck
Arguments:
  b   - Pointer to the button to process.		
Returns: nothing

The input pin is sampled here, and the state machine is implemented to
handled debounce timing.  Timing of the state transitions and wait times are
based on scope captures of switch bounce for the tactile switches that were
used.  BUTTON_PERIOD is defined above as a period long enough that all noise
from bounces should damp out.

The state machine updates status fields in the given pointer to indicate the
active state of the button (whether pushed or not) and whether that state
has changed since the last time the check occurred (trigger some action by
the state machine).
---------------------------------------------------------------------------*/
void ButtonCheck(button* b)
{
  byte raw;
  uint32_t t0, dt;

  raw = digitalRead(b->pin);
  b->prev = b->pressed;

  t0 = millis();
  dt = t0 - b->tref;

  switch (b->state) {
    case BSTATE_UNPRESSED:
      if (LOW == raw) {
        b->tref = t0;
        b->state = BSTATE_DEBOUNCE;
      }
      else {
        b->pressed = false;
      }
      break;

    case BSTATE_DEBOUNCE:
      if (dt > BUTTON_PERIOD) {
        if (LOW == raw) {
          b->tref = t0;
          b->state = BSTATE_PRESSED;
          b->pressed = true;
        }
        else {
          b->state = BSTATE_UNPRESSED;
        }
      }
      break;

    case BSTATE_PRESSED:
      if (LOW == raw) {
        b->pressed = true;
      }
      else {
        b->pressed = false;
        b->state = BSTATE_UNPRESSED;
      }
      break;
  }

  if (b->pressed != b->prev)
    b->changed = true;
}

/*---------------------------------------------------------------------------
Function: ButtonClear
Arguments:
  b  - Indicates the button to clear state on.
Returns: nothing

This function originally was intended to manipulate state data, like the
reference time and pressed state.  Application logic did not require changes
to data beyond just the changed flag.  But this function was kept because it
was an easy, common, single point to make this change at when code already
was written to call into this function.
---------------------------------------------------------------------------*/
void ButtonClear(button* b)
{
  b->changed = false;
}


/*---------------------------------------------------------------------------*/
//
// "Rendering" functions
//
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------
Function: ColorDim
Arguments:
  x   - Copy of the color structure to dim (passed byval)
Returns: modified copy of the original color value.

The ColorDim function applies the same dimming to all LEDs.  Since the
Adafruit_NeoPixel library has a couple quirks and limitations, I did not want
to rely on its brightness capabilities.  Empirically I determined that the
neopixels, running at full brightness, would run down the charge on the SLA
batteries we were using in approximately 12 minutes.  After characterizing
the discharge curve of the batteries, and looking at power consumption by
the number of pixels in the costume, a brightness value was determined that
the battery could operate at and hold up the costume for an hour per battery
charge.  This values is defined for BRIGHTNESS above.
---------------------------------------------------------------------------*/
color ColorDim(color x)
{
  x.r = byte(BRIGHTNESS * x.r);
  x.g = byte(BRIGHTNESS * x.g);
  x.b = byte(BRIGHTNESS * x.b);
  return x;
}

/*---------------------------------------------------------------------------
Function: SegmentFill
Arguments:
  s   - Pointer to a segment in the pixel map to fill with the given color.
  pc  - Pointer to a color structure to write to all pixels in the segment.
Returns: nothing

Used to perform a flood fill operation on the given segment.
---------------------------------------------------------------------------*/
void SegmentFill(const segment* s, const color* pc)
{
  color c;
  int i, k;

  // Handle color ordering based on segment details
  if (s->fixed)
    c = s->color;
  else c = *pc;

  // Fill the segment with the color
  k = s->start;
  for (i = 0; i < s->count; i++, k++)
    s->ios->setPixelColor(k, c.r, c.g, c.b);
}

/*---------------------------------------------------------------------------
Function: SegmentFillAll
Arguments:
  pc  - Pointer to a color structure indicating the color to use.
Returns: nothing

Performs an iterative flood fill on all non-fixed pixel segments in the
pixel map.
---------------------------------------------------------------------------*/
void SegmentFillAll(const color* pc)
{
  const segment* s;
  for (s = Segments; s->ios; s++)
    SegmentFill(s, pc);
}

/*---------------------------------------------------------------------------
Function: SegmentPoke
Arguments:
  s  - Indicates the segment to poke a color into
  k  - Indicates the zero-based index of the pixel to updateLength
  pc - Pointer to the color to assign to that pixel position
Returns: nothing

Allows updating of a single pixel within a segment in the underlying stream
object.  The caller-provided value of k indicates the zero-based offset of
the desired pixel within the segment.  The start index of the segment from
the pixel map is added to access the appropriate position in the underlying
stream.
---------------------------------------------------------------------------*/
void SegmentPoke(const segment* s, int k, const color* pc)
{
  k += s->start;
  s->ios->setPixelColor(k, pc->r, pc->g, pc->b);
}


/*---------------------------------------------------------------------------*/
//
// Implementations for the state machine
//
// Note on state handler functions: The single argument, init, indicates
// whether the state is being called because of a new state transition,
// allowing initialization of state data to occur at the state transition.
// When actively in the state represented by the handler, init should always
// be false.
//
// Each state handler is responsible for managing local static variables to
// represent persistent data specific to that state.
//
/*---------------------------------------------------------------------------*/

// Prototypes for wrapping handler functionality
bool HandleButtons(int* next);
bool HandleBrightButton();
void HandleStateTransition(int toState);


/*---------------------------------------------------------------------------
Function: StateDamage
Arguments:
  init  - Is the handler being called to initialize for a state transition?
Returns: nothing

Handles updating all color-change pixels to damage color mode.  Originally
this was intended to perform a smooth gradient transition from black to
damage color, but when the final pixel count for the costume was arrived at,
SMOOTHLY updating every color change pixel in the costume within a transition
period appropriate for the effect was not feasible given the protocol timing
constraints and that we were running out of time before the contest.  So the
substate machine in this function was originally designed to handle that
gradiation, until the effect was abandoned.  State 0 is where that gradient
would have been implemented, but instead it just skips to filling with the
appropriate color.
---------------------------------------------------------------------------*/
void StateDamage(bool init)
{
  static int state, next;
  static unsigned tref;
  unsigned t0, dt;
  color c;
  
  if (init) {
    AppState = STATE_DAMAGE;
    state = 0;
    tref = millis();
  }
  else {
    t0 = millis();
    switch (state) {
      case 0:
        // Fading from black into the appropriate color
        state++;
        tref = t0;
        Redraw = true;
        break;

      case 1:
        // Check buttons and periodically refresh all pixels
        if (HandleButtons(&next)) {
          tref = t0;
          state = 2;
        }
        else if (HandleBrightButton()) {
          Redraw = true;
          tref = t0;
        }
        else {
          dt = t0 - tref;
          if (dt > REDRAW_PERIOD) {
            Redraw = true;
            tref = t0;
          }
        }
        break;

      case 2:
        // Fade from steady to black and switch to next state.  Since we're
		// not doing the gradient, as originally intended, we just transition
		// to the new state.
        c = {0,0,0};
        Redraw = true;
        HandleStateTransition(next);
        break;
    }

	// If logic above dictates that a redraw is required then handle updating
	// all pixels appropriately here in preparation for the write operation
	// outside of the state machine.
    if (Redraw) {
      if (Bright)
        c = NormalTab[0];
      else c = ColorDim(NormalTab[0]);
      SegmentFillAll(&c);
    }
  }
}

/*---------------------------------------------------------------------------
Function: StateHealing
Arguments:
  init  - Is the handler being called to initialize for a state transition?
Returns: nothing

Pretty much a carbon copy of the StateDamage state, just for healing color
mode.
---------------------------------------------------------------------------*/
void StateHealing(bool init)
{
  static int state, next;
  static unsigned tref;
  unsigned t0, dt;
  color c;
  
  if (init) {
    AppState = STATE_HEALING;
    tref = millis();
    state = 0;
  }
  else {
    t0 = millis();
    switch (state) {
      case 0:
        // Fading from black to steady state color
        state++;
        tref = t0;
        Redraw = true;
        break;

      case 1:
        // Holding at steady state color
        if (HandleButtons(&next)) {
          tref = t0;
          state = 2;
        }
        else if (HandleBrightButton()) {
          Redraw = true;
          tref = t0;
        }
        else {
          dt = t0 - tref;
          if (dt > REDRAW_PERIOD) {
            Redraw = true;
            tref = t0;
          }
        }
        break;

      case 2:
        // Transitioning to black before switching to another state
        // Fade from steady to black and switch to next state.
        c = {0,0,0};
        Redraw = true;
        HandleStateTransition(next);
        break;
    }
    
	// If logic above dictates that a redraw is required then handle updating
	// all pixels appropriately here in preparation for the write operation
	// outside of the state machine.
    if (Redraw) {
      if (Bright)
        c = NormalTab[1];
      else c = ColorDim(NormalTab[1]);
      SegmentFillAll(&c);
    }
  }
}

/*---------------------------------------------------------------------------
Function: StateUltimate
Arguments:
  init  - Is the handler being called to initialize for a state transition?
Returns: nothing

When in ultimate mode, the state handler will cycle through the piecewise,
periodic function defined above in the UltTab global table.  As implemented
in this costume, each pixel in the series gets assigned a color sequentially
from the table at a rate of 1 pixel = 1 interval in the table.  When the
function has walked through the table, it starts over at entry 0 and
continues writing to pixels in the pixel map, giving the periodic effect of
repeated color patterns.  A phase shift, phi, which increments every
ULTIMATE_PERIOD milliseconds, is added to the table index as it is being
walked.  This gives N possible phase offsets, where N is the number of
intervals in the piecewise function.  With phi updating periodically, the
effect is that the periodic pattern chases down the sequence at a rate of
1 pixel per ULTIMATE_PERIOD milliseconds.
---------------------------------------------------------------------------*/
void StateUltimate(bool init)
{
  static int state;     // Local substate machine variable
  static int next;      // Indicates the next state to transition to based on button presses
  static int phi;       // Phase shift offset for the array index
  static unsigned tref; // Time reference for phase shift rotation
  static color a[8];    // Local array copy of the ultimate function, in the
                        // color brightness appropriate for the brightness mode.
  unsigned t0, dt;
  const segment* s;
  int i, k;
  color *c;

  if (init) {
    AppState = STATE_ULTIMATE;
    state = 0;
    tref = millis();
  }
  else {
    t0 = millis();
    switch (state) {
      case 0:
		// Additional initialization of data for the state.
        state++;
        tref = t0;
        phi = 0;
        Redraw = true;
        if (Bright) {
          for (k = 0; k < 8; k++)
            a[k] = UltTab[k];
        }
        else {
          for (k = 0; k < 8; k++)
            a[k] = ColorDim(UltTab[k]);
        }
        break;
        
      case 1:
	    // Check for state transition signals based on button inputs
		// first.  If no state transition is called for by the user
		// inputs then redraw the string periodically as phase changes.
        if (HandleButtons(&next)) {
          if (STATE_ULTIMATE == next)
            state = 2;
        }
        else if (HandleBrightButton()) {
          if (Bright) {
            for (k = 0; k < 8; k++)
              a[k] = UltTab[k];
          }
          else {
            for (k = 0; k < 8; k++)
              a[k] = ColorDim(UltTab[k]);
          }
          Redraw = true;
        }
        else {
		  // Handle adjusting phase and redrawing if the update period
		  // has expired.
          dt = t0 - tref;
          if (dt > ULTIMATE_PERIOD) {
            tref = t0;
            phi--;
            if (phi < 0)
              phi = 7;
            Redraw = true;
          }
        }
        break;
        
      case 2:
	    // Based on button input a state transition needs to occur.  Let
		// the state machine initialize the next state and handle the
		// transition.
        HandleStateTransition(STATE_DAMAGE);
        break;
    }

	// If logic from above requires, then apply colors to all of the pixels
	// in the map in preparation for writing.
    if (Redraw) {
      k = 0;
      for (s = Segments; s->ios; s++) {
        if (s->fixed)
          continue;
        for (i = 0; i < s->count; i++) {
          c = &a[(k + phi) % 8];
          SegmentPoke(s, i, c);
          k++;
        }
      }
    }
  }
}

/*---------------------------------------------------------------------------
Function: StatePride
Arguments:
  init  - Is the handler being called to initialize for a state transition?
Returns: nothing

Operates on the same principal as StateUltimate to produce a chasing effect
based on the RainbowTab periodic function table above.  See the description
of StateUltimate for a description of the animation function.  The period
with which the phase changes is PRIDE_PERIOD instead of ULTIMATE_PERIOD.
---------------------------------------------------------------------------*/
void StatePride(bool init)
{
  static int state, next, phi;
  static unsigned tref;
  static color a[7];
  unsigned t0, dt;
  const segment *s;
  int i, k;
  color* c;

  if (init) {
    AppState = STATE_PRIDE;
    state = 0;
    tref = millis();
  }
  else {
    t0 = millis();
    switch (state) {
      case 0:
	    // Initialize more data.
        state++;
        tref = t0;
        phi = 0;
        Redraw = true;
        if (Bright) {
          for (k = 0; k < 7; k++)
            a[k] = RainbowTab[k];
        }
        else {
          for (k = 0; k < 7; k++)
            a[k] = ColorDim(RainbowTab[k]);
        }
        break;

      case 1:
	    // Begin by checking button inputs.  If the buttons have changed
		// then the user is calling for a state transition.  The substate
		// 2 case handles the state transition.
        if (HandleButtons(&next)) {
          if (STATE_PRIDE == next)
            state = 2;
        }
        else if (HandleBrightButton()) {
		  // If the brightness button has been pressed then the brightness
		  // has changed.  So the local table copy needs to update based on
		  // brightness.
          if (Bright) {
            for (k = 0; k < 7; k++)
              a[k] = RainbowTab[k];
          }
          else {
            for (k = 0; k < 7; k++)
              a[k] = ColorDim(RainbowTab[k]);
          }
          Redraw = true;
        }
        else {
		  // Handle changing the phase of the table walk if the PRIDE_PERIOD
		  // wait period has elapsed.
          dt = t0 - tref;
          if (dt > PRIDE_PERIOD) {
            tref = t0;
            phi++;
            if (phi >= 7)
              phi = 0;
            Redraw = true;
          }
        }
        break;

      case 2:
	    // Transition to the appropriate state.
        HandleStateTransition(STATE_DAMAGE);
        break;
    }

    if (Redraw) {
      k = 0;
      for (s = Segments; s->ios; s++) {
        if (s->fixed)
          continue;
        for (i = 0; i < s->count; i++) {
          c = &a[(k + phi) % 7];
          SegmentPoke(s, i, c);
          k++;
        }
      }
    }
  }
}

/*---------------------------------------------------------------------------
Function: StateIrish
Arguments:
  init  - Is the handler being called to initialize for a state transition?
Returns: nothing

Causes the entire costume to be painted in the colors of the Irish flag.  The
pixels in the tubes only are split into thirds and painted one third each a
color in the flag.  The rest of the pixels are blanked out to black, since
the tubes are the only place that the pattern would be recognizable.
---------------------------------------------------------------------------*/
void StateIrish(bool init)
{
  static int state, next;
  static unsigned tref;
  unsigned t0, dt;
  color c1, c2, c3;
  const segment* s;
  int k;

  if (init) {
    AppState = STATE_IRISH;
    state = 0;
    tref = millis();
  }
  else {
    t0 = millis();
    switch (state) {
      case 0:
	    // Initialization and redraw
        state++;
        Redraw = true;
        tref = t0;
        break;

      case 1:
	    // Handle state transitions that would be indicated by changes in
		// button states.
        if (HandleButtons(&next)) {
          if (STATE_IRISH == next)
            state = 2;
        }
        else if (HandleBrightButton()) {
		  // Trigger a redraw with the appropriate brightness.  The brightness
		  // will be computed below in the redraw code.
          Redraw = true;
          tref = t0;
        }
        else {
		  // Handle periodic redraws
          dt = t0 - tref;
          if (dt > REDRAW_PERIOD) {
            Redraw = true;
            tref = t0;
          }
        }
        break;

      case 2:
        HandleStateTransition(STATE_DAMAGE);
        break;
    }

    if (Redraw) {
	  // Start by blanking all pixels to black.
      c1 = {0,0,0};
      SegmentFillAll(&c1);

	  // Determine new colors based on the brightness flag.
      if (Bright) {
        c1 = IrelandTab[0];
        c2 = IrelandTab[1];
        c3 = IrelandTab[2];
      }
      else {
        c1 = ColorDim(IrelandTab[0]);
        c2 = ColorDim(IrelandTab[1]);
        c3 = ColorDim(IrelandTab[2]);
      }
      
	  // Redraw all appropriate pixels.
      s = Segments;
      for (k = 0; k < s->count; k++) {
        if (k < (s->count / 3))
          SegmentPoke(s, k, &c1);
        else if (k < (2 * s->count / 3))
          SegmentPoke(s, k, &c2);
        else SegmentPoke(s, k, &c3);
      }
    }
  }
}

/*---------------------------------------------------------------------------
Function: StateMachine
Arguments: none
Returns: nothing

Top level state machine implementation.  This function represents a single
entry point the app can use once other IO operations have completed to
invoke the state machine.
---------------------------------------------------------------------------*/
void StateMachine()
{
  switch (AppState) {
    case STATE_NONE:
      HandleStateTransition(STATE_DAMAGE);
      break;

    case STATE_DAMAGE:
      StateDamage(false);
      break;

    case STATE_HEALING:
      StateHealing(false);
      break;

    case STATE_ULTIMATE:
      StateUltimate(false);
      break;

    case STATE_PRIDE:
      StatePride(false);
      break;

    case STATE_IRISH:
      StateIrish(false);
      break;
  }
}


/*---------------------------------------------------------------------------*/
//
// Implementations of handler wrapper functions
//
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------
Function: HandleButtons
Arguments:
  next  - Pointer to a caller-allocated integer to receive a value indicating
          the appropriate state to transition to if a button input event
		  requires a state transition.
Returns: boolean flag indicating whether a state transition is required.

This function checks in a single call the buttons that trigger a change to
a different drawing pattern in the costume.  If the return value is false,
the caller should assume that the value contained within *next is invalid
and should not use it.  If the return value is true then the caller must
assume a button press indicated that a draw mode change has been requested,
and look at the *next value to see which state we are going to.
---------------------------------------------------------------------------*/
bool HandleButtons(int* next)
{
  bool y;

  y = false;
  if (UltBtn.changed) {
    if (UltBtn.pressed) {
      y = true;
      *next = STATE_ULTIMATE;
    }
    ButtonClear(&UltBtn);
  }
  else if (PrideBtn.changed) {
    if (PrideBtn.pressed) {
      y = true;
      *next = STATE_PRIDE;
    }
    ButtonClear(&PrideBtn);
  }
  else if (IrishBtn.changed) {
    if (IrishBtn.pressed) {
      y = true;
      *next = STATE_IRISH;
    }
    ButtonClear(&IrishBtn);
  }
  else if (TogBtn.changed) {
    if (TogBtn.pressed) {
      y = true;
      if (STATE_HEALING == AppState)
        *next = STATE_DAMAGE;
      else *next = STATE_HEALING;
    }
    ButtonClear(&TogBtn);
  }

  return y;
}

/*---------------------------------------------------------------------------
Funtion: HandleBrightButton
Arguments: none
Returns: Value indicating if a brightness change needs to occur.

This function handles servicing the bright mode toggle button.  If the
bright mode button has been pressed, then the value of the Bright global
flag is toggled, and a return value of true is set.  This function only
serves to indicate to a state handler function whether it needs to do
something to change the color scheme being rendered to reflect the requested
brightness.
---------------------------------------------------------------------------*/
bool HandleBrightButton()
{
  bool y;

  y = false;
  if (BrightBtn.changed) {
    if (BrightBtn.pressed) {
      y = true;
      Bright = !Bright;
    }
    ButtonClear(&BrightBtn);
  }

  return y;
}

/*---------------------------------------------------------------------------
Function: HandleStateTransition
Arguments:
  toState - Indicates the state that needs to be transitioned into.
Returns: nothing

This is a call funnel function, providing a single place that code can call
into anywhere to initiate a state transition.  The only place that a state
handler function should be called with init=true should be from here.  Any
other invocation of a state handler function should be called with
init=false.  When the initializer call has been made to the state handler,
it should update the global state variable to indicate that it is entering
and remaining in the new state, rather than transitioning to another state
because of exceptional conditions of some sort.
---------------------------------------------------------------------------*/
void HandleStateTransition(int toState)
{
  switch (toState) {
    case STATE_NONE:
      break;

    case STATE_DAMAGE:
      StateDamage(true);
      break;

    case STATE_HEALING:
      StateHealing(true);
      break;

    case STATE_ULTIMATE:
      StateUltimate(true);
      break;

    case STATE_PRIDE:
      StatePride(true);
      break;

    case STATE_IRISH:
      StateIrish(true);
      break;
  }
}


/*---------------------------------------------------------------------------*/
//
// Externally defined functions we must provide
//
/*---------------------------------------------------------------------------*/

void setup() {
  // put your setup code here, to run once:
  ButtonInit(&TogBtn, TOGGLE_BUTTON);
  ButtonInit(&UltBtn, ULT_BUTTON);
  ButtonInit(&BrightBtn, BRIGHT_BUTTON);
  ButtonInit(&PrideBtn, PRIDE_BUTTON);
  ButtonInit(&IrishBtn, IRISH_BUTTON);

  Backpack.setPin(PIXEL_PIN_BACKPACK);
  Backpack.updateType(PIXEL_MODE);
  Backpack.updateLength(63);
  Backpack.begin();

  Suit.setPin(PIXEL_PIN_SUIT);
  Suit.updateType(PIXEL_MODE);
  Suit.updateLength(118);
  Suit.begin();

  pinMode(PIN_HEARTBEAT, OUTPUT);
  digitalWrite(PIN_HEARTBEAT, LOW);

  AppState = STATE_NONE;
  Redraw = false;
  Sending1 = false;
  Sending2 = false;
  Bright = false;
}

void loop() {
  // Get input data
  ButtonCheck(&TogBtn);
  ButtonCheck(&UltBtn);
  ButtonCheck(&BrightBtn);
  ButtonCheck(&PrideBtn);
  ButtonCheck(&IrishBtn);

  // All intelligence is handled in the state machine on a per-state
  // basis, after acquiring input data from buttons.  Rendering and
  // state transitions are all handled here.  No IO is done however.
  StateMachine();

  // If indicated, write data that was rendered in the state machine
  // to the pixels.  Clear the redraw flag immediately so that we
  // don't have state handlers rewriting data repeatedly.  Use the
  // Sending flag to track progress writing to LEDs.
  if (Redraw) {
    Redraw = false;
    Sending1 = true;
    Sending2 = false;
  }

  if (Sending2 && Suit.canShow()) {// && Suit.canShow()) {
    Sending2 = false;
    Suit.show();
    digitalWrite(PIN_HEARTBEAT, LOW);
  }

  // If we are supposed to be sending LED data then do it here.
  if (Sending1 && Backpack.canShow()) {// && Suit.canShow()) {
    Sending1 = false;
    Sending2 = true;
    digitalWrite(PIN_HEARTBEAT, HIGH);
    Backpack.show();
  }
}

/* End of file */
