package bg.sofia.uni.fmi.mjt.eventbus.events;

import bg.sofia.uni.fmi.mjt.eventbus.models.Action;

public record ActionPayload(Action action) implements Payload<Action> {
    @Override
    public int getSize() {
        return action.name().length();
    }

    @Override
    public Action getPayload() {
        return action;
    }
}
