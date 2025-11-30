package bg.sofia.uni.fmi.mjt.file.step;

import bg.sofia.uni.fmi.mjt.file.File;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

class UpperCaseFileTest {
    @Test
    void testProcessNullFileThrows() {
        assertThrows(IllegalArgumentException.class, () -> new UpperCaseFile().process(null),
                     "The process method should throw an exception when given a null File object");
    }

    @Test
    void testProcessFileWithNullContentThrows() {
        File file = new File("test");
        file.setContent(null);

        assertThrows(IllegalArgumentException.class, () -> new UpperCaseFile().process(file),
                     "The process method should throw an exception when given a File object with null content");
    }

    @Test
    void testProcessReturnsUpperCaseFile() {
        File file = new File("test");
        File output = new UpperCaseFile().process(file);

        assertEquals("TEST", output.getContent(),
                     "The process method should return a File object with uppercase content");
    }

    @Test
    void testProcessReturnsUpperCaseFileWithEmptyContent() {
        File file = new File("");

        File output = new UpperCaseFile().process(file);
        assertEquals("", output.getContent(), "The process method should return a File object with empty content");
    }

    @Test
    void testProcessReturnsUpperCaseContentForFileWithUpperCaseContent() {
        File file = new File("TEST");

        File output = new UpperCaseFile().process(file);
        assertEquals("TEST", output.getContent(), "The process method should return a File object with empty content");
    }

    @Test
    void testProcessReturnsNewFileInstance() {
        File file = new File("test");
        File output = new UpperCaseFile().process(file);

        assertNotSame(file, output, "The process method should return the same File object instance");
    }
}
