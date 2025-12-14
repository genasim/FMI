package bg.sofia.uni.fmi.mjt.order.loader;

import bg.sofia.uni.fmi.mjt.order.domain.Category;
import bg.sofia.uni.fmi.mjt.order.domain.PaymentMethod;
import bg.sofia.uni.fmi.mjt.order.domain.Status;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class EnumParserTest {
    @Test
    void testParseNullEnumTypeThrows() {
        assertThrows(IllegalArgumentException.class, () -> EnumParser.parse(null, "Test"),
                     "Should throw IllegalArgumentException when enum type is null");
    }

    @Test
    void testParseNullEnumStringConstantThrows() {
        assertThrows(IllegalArgumentException.class, () -> EnumParser.parse(PaymentMethod.class, null),
                     "Should throw IllegalArgumentException when raw enum constant is null");
    }

    @Test
    void testParseBlankEnumStringConstantThrows() {
        assertThrows(IllegalArgumentException.class, () -> EnumParser.parse(PaymentMethod.class, " "),
                     "Should throw IllegalArgumentException when raw enum constant is blank");
    }

    @Test
    void testParseStringConstantNotInEnumThrows() {
        assertThrows(IllegalArgumentException.class, () -> EnumParser.parse(Category.class, "Random"),
                     "Should throw IllegalArgumentException when raw enum constant is blank");
    }

    @Test
    void testParseValidEnumStringMultipleWordsReturnsEnumValue() {
        assertEquals(PaymentMethod.CREDIT_CARD, EnumParser.parse(PaymentMethod.class, "Credit Card"));
    }

    @Test
    void testParseValidEnumStringSingleWordReturnsEnumValue() {
        assertEquals(Status.PENDING, EnumParser.parse(Status.class, "pending"));
    }
}
