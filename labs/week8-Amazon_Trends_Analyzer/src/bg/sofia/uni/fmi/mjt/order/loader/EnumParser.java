package bg.sofia.uni.fmi.mjt.order.loader;

import java.util.Arrays;
import java.util.stream.Collectors;

public class EnumParser {
    private EnumParser() {
    }

    public static <T extends Enum<T>> T parse(Class<T> enumType, String raw) {
        if (enumType == null) {
            throw new IllegalArgumentException("Enum type cannot be null");
        }

        if (raw == null) {
            throw new IllegalArgumentException("Raw enum value cannot be null");
        }

        if (raw.isBlank()) {
            throw new IllegalArgumentException("Raw enum value cannot be blank");
        }

        String normalized = Arrays.stream(raw.trim().split("\\s+"))
            .filter(s -> !s.isBlank())
            .map(String::toUpperCase)
            .collect(Collectors.joining("_"));

        return Enum.valueOf(enumType, normalized);
    }
}
