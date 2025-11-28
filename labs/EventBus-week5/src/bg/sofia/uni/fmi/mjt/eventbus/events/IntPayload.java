package bg.sofia.uni.fmi.mjt.eventbus.events;

public record IntPayload(int integer) implements Payload<Integer> {
    @Override
    public int getSize() {
        return Integer.SIZE;
    }

    @Override
    public Integer getPayload() {
        return integer;
    }
}
