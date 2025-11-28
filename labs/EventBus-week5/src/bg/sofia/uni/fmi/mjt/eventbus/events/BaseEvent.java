package bg.sofia.uni.fmi.mjt.eventbus.events;

import java.time.Instant;

public abstract class BaseEvent implements Event<Payload<?>> {
    private final Instant timestamp = Instant.now();

    protected Payload<?> payload;
    protected String source;
    protected int priority;

    protected BaseEvent(Payload<?> payload, String source, int priority) {
        this.source = source;
        this.priority = priority;
        this.payload = payload;
    }

    @Override
    public int getPriority() {
        return priority;
    }

    @Override
    public Payload<?> getPayload() {
        return payload;
    }

    @Override
    public String getSource() {
        return source;
    }

    @Override
    public Instant getTimestamp() {
        return timestamp;
    }

    public void setPriority(int priority) {
        this.priority = priority;
    }

    public void setPayload(Payload<?> payload) {
        this.payload = payload;
    }

    public void setSource(String source) {
        this.source = source;
    }
}
