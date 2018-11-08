struct list { int head; };

enum Sec {
    Low,
    High
};

enum Button {
    ButtonLow,
    ButtonHigh,
    ButtonOverlay
};

enum Domain {
    DomainLow,
    DomainHigh,
    DomainOverlay,
    DomainInvalid
};

enum EventType {
    EventTypeMouse,
    EventTypeNone,
    EventTypeKeyboard
};

enum Event {
    EventNone,
    EventMouseDown
};

int test = 0;
int max(int n, int m);
int min(int n, int m) {}