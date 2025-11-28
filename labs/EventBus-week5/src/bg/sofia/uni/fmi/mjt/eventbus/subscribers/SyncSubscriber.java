package bg.sofia.uni.fmi.mjt.eventbus.subscribers;

import bg.sofia.uni.fmi.mjt.eventbus.events.Event;

public class SyncSubscriber<T extends Event<?>> implements Subscriber<T> {
    @Override
    public void onEvent(T event) {
        System.out.printf("""
                              Event received: %s
                              Receiver: %s
                              Payload: %s
                              Timestamp: %s
                              Priority: %d
                              Source: %s
                              
                              """,
                          event.getClass().getSimpleName(),
                          this,
                          event.getPayload(),
                          event.getTimestamp(),
                          event.getPriority(),
                          event.getSource());
    }
}
