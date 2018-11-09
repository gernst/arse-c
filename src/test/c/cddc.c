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

void driver() {
    bool temp;
    bool done_rpc;
    bool switch_state_mouse_down = false;
    Domain overlay_result;
    Domain cursor_domain;

    *current_event_data = EventNone;
    *indicated_domain = *active_domain;

    *hid_current_event_type = EventTypeNone;
    
    while(true) {
        lock(hid_read_atomicity_lock);
        temp = *hid_mouse_available;
        unlock(hid_read_atomicity_lock);

        if(temp) {
            *hid_current_event_type = EventTypeMouse;

            lock(hid_read_atomicity_lock);
            *current_event_data = *hid_mouse_source;
            unlock(hid_read_atomicity_lock);

            lock(rpc_overlay_mouse_click_lock);
            *rpc_overlay_mouse_click_arg = *current_event_data;
            *rpc_overlay_mouse_click_call = true;
            unlock(rpc_overlay_mouse_click_lock);

            done_rpc = false;
            while(!done_rpc) {
                lock(rpc_overlay_mouse_click_lock);
                if(!*rpc_overlay_mouse_click_call) {
                    overlay_result = *rpc_overlay_mouse_click_ret;
                    done_rpc = true;
                }
                unlock(rpc_overlay_mouse_click_lock);
            }

            if(overlay_result != DomainInvalid) {
                cursor_domain = DomainOverlay;
            } else {
                *compositor_cursor_position = *current_event_data;

                lock(compositor_read_atomicity_lock);
                cursor_domain = *compositor_domain_under_cursor;
                unlock(compositor_read_atomicity_lock);

                if(cursor_domain == DomainInvalid) {
                    cursor_domain = *active_domain;
                }
            }

            if(cursor_domain == DomainOverlay) {
                if(overlay_result != DomainOverlay &&
                   overlay_result != DomainInvalid &&
                   *current_event_data == EventMouseDown &&
                   !switch_state_mouse_down &&
                   overlay_result != *active_domain)
                {
                    *active_domain = overlay_result;
                    *indicated_domain = *active_domain;
                }
            } else {
                if(*current_event_data == EventMouseDown &&
                   !switch_state_mouse_down &&
                   overlay_result != *active_domain)
                {
                    *active_domain = cursor_domain;
                    *indicated_domain = *active_domain;
                }

                if(switch_state_mouse_down ||
                   *current_event_data == EventMouseDown)
                {
                    if(*active_domain == DomainLow) {
                        *output_event_buffer0 = *current_event_data;
                    } else {
                        *output_event_buffer1 = *current_event_data;
                    }
                } else {
                    if(cursor_domain == DomainLow) {
                        *output_event_buffer0 = *current_event_data;
                    } else {
                        *output_event_buffer1 = *current_event_data;
                    }
                }
            }

            if(*current_event_data == EventMouseDown) {
                switch_state_mouse_down = true;
            } else {
                switch_state_mouse_down = false;
            }
        }

        lock(hid_read_atomicity_lock);
        temp = *hid_keyboard_available;
        unlock(hid_read_atomicity_lock);

        if(temp) {
            *current_event_data = EventNone;
            *hid_current_event_type = EventTypeKeyboard;

            if(*indicated_domain == DomainHigh) {
                lock(hid_read_atomicity_lock);
                *current_event_data = *hid_high_keyboard_source;
                unlock(hid_read_atomicity_lock);
            } else {
                lock(hid_read_atomicity_lock);
                *current_event_data = *hid_low_keyboard_source;
                unlock(hid_read_atomicity_lock);
            }

            if(*active_domain == DomainLow) {
                *output_event_buffer0 = *current_event_data;
            } else {
                *output_event_buffer1 = *current_event_data;
            }
        }

        *current_event_data = EventNone;
        *hid_current_event_type = EventTypeNone;
    }
}



