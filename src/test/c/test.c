typedef int bool;
typedef struct Lock Lock;

void lock(Lock *l);
void unlock(Lock *l);

typedef enum {
    Low,
    High
} Sec;

typedef enum {
    ButtonLow,
    ButtonHigh,
    ButtonOverlay
} Button;

typedef enum {
    DomainLow,
    DomainHigh,
    DomainOverlay,
    DomainInvalid
} Domain;

typedef enum {
    EventTypeMouse,
    EventTypeNone,
    EventTypeKeyboard
} EventType;

typedef enum {
    EventNone,
    EventMouseDown
} Event;

Lock      *rpc_overlay_mouse_click_lock;
bool      *rpc_overlay_mouse_click_call;
Button    *rpc_overlay_mouse_click_arg;
Domain    *rpc_overlay_mouse_click_ret;

Lock      *hid_read_atomicity_lock;
bool      *hid_mouse_available;
bool      *hid_keyboard_available;
EventType *hid_current_event_type;

Event     *hid_mouse_source;
Event     *hid_low_keyboard_source;
Event     *hid_high_keyboard_source;

Event     *current_event_data;
Event     *output_event_buffer0;
Event     *output_event_buffer1;
Domain    *active_domain;
Domain    *indicated_domain;

Lock      *compositor_read_atomicity_lock;
Event     *compositor_cursor_position;
Domain    *compositor_domain_under_cursor;