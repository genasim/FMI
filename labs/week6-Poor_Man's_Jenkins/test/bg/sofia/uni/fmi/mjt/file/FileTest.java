package bg.sofia.uni.fmi.mjt.file;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

class FileTest {
    @Test
    void testFileConstructorNullContentThrowsException() {
        assertThrows(IllegalArgumentException.class, () -> new File(null));
    }

    @Test
    void testGetContentFileWithNullContentReturnsNull() {
        File file = new File("test");
        file.setContent(null);

        assertNull(file.getContent(), "Should return null when file content is null");
    }

    @Test
    void testGetContentFileWithEmptyContentReturnsEmpty() {
        File file = new File("");
        assertEquals("", file.getContent(), "Should return empty string when file content is empty");
    }

    @Test
    void testGetContentFileWithMeaningfulContent() {
        File file = new File("Lorem ipsum dolor sit amet");
        assertEquals("Lorem ipsum dolor sit amet", file.getContent(),
                     "Should return file content as is when it is not empty or null");
    }
}
