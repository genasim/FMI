package bg.sofia.uni.fmi.mjt.eventbus.events;

public class DeleteActionEvent extends BaseEvent {
    public DeleteActionEvent(ActionPayload payload, String source, int priority) {
        if (payload == null) {
            throw new IllegalArgumentException("Payload cannot be null");
        }

        super(payload, source, priority);
    }
}
