package bg.sofia.uni.fmi.mjt.file.step;

import bg.sofia.uni.fmi.mjt.file.File;
import bg.sofia.uni.fmi.mjt.file.exception.EmptyFileException;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

class CheckEmptyFileTest {
    @Test()
    void testProcessNullFileThrowsException() {
        Exception exception = assertThrows(EmptyFileException.class, () -> new CheckEmptyFile().process(null),
                                           "Should throw EmptyFileException");

        String expectedMessage = "Input file or its content is empty or null";
        String actualMessage = exception.getMessage();
        assertEquals(expectedMessage, actualMessage, "Wrong exception message:");
    }

    @Test()
    void testProcessFileWithNullContentThrowsException() {
        final File file = new File("test");
        file.setContent(null);
        Exception exception = assertThrows(EmptyFileException.class, () -> new CheckEmptyFile().process(file),
                                           "Should throw EmptyFileException");

        String expectedMessage = "Input file or its content is empty or null";
        String actualMessage = exception.getMessage();
        assertEquals(expectedMessage, actualMessage, "Wrong exception message:");
    }

    @Test()
    void testProcessFileWithEmptyContentThrowsException() {
        Exception exception = assertThrows(EmptyFileException.class, () -> new CheckEmptyFile().process(new File("")),
                                           "Should throw EmptyFileException");

        String expectedMessage = "Input file or its content is empty or null";
        String actualMessage = exception.getMessage();
        assertEquals(expectedMessage, actualMessage, "Wrong exception message:");
    }

    @Test()
    void testProcessReturnsSameFileInstance() {
        File file = new File("test");
        File output = new CheckEmptyFile().process(file);
        assertSame(file, output, "Should return the same instance");
    }
}
