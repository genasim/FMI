package bg.sofia.uni.fmi.mjt.eventbus.events.comparators;

import bg.sofia.uni.fmi.mjt.eventbus.events.Event;

import java.util.Comparator;

public class EventTimestampComparator implements Comparator<Event<?>> {
    @Override
    public int compare(Event<?> o1, Event<?> o2) {
        return o1.getTimestamp().compareTo(o2.getTimestamp());
    }
}
