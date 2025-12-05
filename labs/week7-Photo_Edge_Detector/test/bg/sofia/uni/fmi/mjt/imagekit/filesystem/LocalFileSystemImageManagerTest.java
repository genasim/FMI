package bg.sofia.uni.fmi.mjt.imagekit.filesystem;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.junit.jupiter.MockitoExtension;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

@ExtendWith(MockitoExtension.class)
class LocalFileSystemImageManagerTest {
    private FileSystemImageManager fileManager;
    private BufferedImage image;

    @BeforeEach
    void setUp() {
        fileManager = new LocalFileSystemImageManager();
        image = new BufferedImage(100, 100, BufferedImage.TYPE_INT_RGB);
    }

    @Test
    void testLoadImageNullFilePathThrows() {
        assertThrows(IllegalArgumentException.class, () -> fileManager.loadImage(null),
                     "Should throw IllegalArgumentException when loading image from null file path");
    }

    @Test
    void testLoadImageFilePathIsNotFileThrows(@TempDir Path tempDir) {
        Path filePath = tempDir.resolve("./test/nested");
        assertThrows(IOException.class, () -> fileManager.loadImage(filePath.toFile()),
                     "Should throw IOException when loading image from non-file file path");
    }

    @Test
    void testLoadImageSupportedFormatJPG(@TempDir Path tempDir) throws IOException {
        Path filePath = tempDir.resolve("test.jpg");
        Files.writeString(filePath, "");
        assertDoesNotThrow(() -> fileManager.loadImage(filePath.toFile()),
                           "Should not throw IOException when loading image with supported format .jpg");
    }

    @Test
    void testLoadImageSupportedFormatPNG(@TempDir Path tempDir) throws IOException {
        Path filePath = tempDir.resolve("test.png");
        Files.writeString(filePath, "");
        assertDoesNotThrow(() -> fileManager.loadImage(filePath.toFile()),
                           "Should not throw IOException when loading image with supported format .png");
    }

    @Test
    void testLoadImageSupportedFormatJPEG(@TempDir Path tempDir) throws IOException {
        Path filePath = tempDir.resolve("test.jpeg");
        Files.writeString(filePath, "");
        assertDoesNotThrow(() -> fileManager.loadImage(filePath.toFile()),
                           "Should not throw IOException when loading image with supported format .jpeg");
    }

    @Test
    void testLoadImageSupportedFormatBMP(@TempDir Path tempDir) throws IOException {
        Path filePath = tempDir.resolve("test.bmp");
        Files.writeString(filePath, "");
        assertDoesNotThrow(() -> fileManager.loadImage(filePath.toFile()),
                           "Should not throw IOException when loading image with supported format .bmp");
    }

    @Test
    void testLoadImageUnsupportedFormatThrows(@TempDir Path tempDir) throws IOException {
        Path filePath = tempDir.resolve("test.txt");
        Files.writeString(filePath, "");
        assertThrows(IOException.class, () -> fileManager.loadImage(filePath.toFile()),
                     "Should throw IOException when loading image with unsupported format");
    }

    @Test
    void testLoadImageMissingFormatThrows(@TempDir Path tempDir) throws IOException {
        Path filePath = tempDir.resolve("random-file");
        Files.writeString(filePath, "");
        assertThrows(IOException.class, () -> fileManager.loadImage(filePath.toFile()),
                     "Should throw IOException when loading image with unsupported format");
    }

    @Test
    void testLoadImageMissingFormatEndsWithSupportedFormatSubstringThrows(@TempDir Path tempDir) throws IOException {
        Path filePath = tempDir.resolve("testpng");
        Files.writeString(filePath, "");
        assertThrows(IOException.class, () -> fileManager.loadImage(filePath.toFile()),
                     "Should throw IOException when loading image with unsupported format");
    }

    @Test
    void testSaveImageNullFilePathThrows() {
        assertThrows(IllegalArgumentException.class,
                     () -> fileManager.saveImage(image, null),
                     "Should throw IllegalArgumentException when saving image to null file path");
    }

    @Test
    void testSaveImageNullImageThrows() {
        assertThrows(IllegalArgumentException.class, () -> fileManager.saveImage(null, new File("")),
                     "Should throw IllegalArgumentException when saving image to null file path");
    }

    @Test
    void testSaveImageParentDirectoryDoesntExistThrows() throws IOException {
        File saveFile = new File("dir/test/file");
        assertThrows(FileNotFoundException.class, () -> fileManager.saveImage(image, saveFile),
                     "Should throw FileNotFoundException when saving image to non-existent directory");
    }

    @Test
    void testSaveImageOutputFilePathAlreadyExistsThrows(@TempDir Path tempDir) throws IOException {
        Path tempFile = tempDir.resolve("test.png");
        Files.createFile(tempFile);

        File output = tempDir.resolve("test.png").toFile();
        assertThrows(FileAlreadyExistsException.class, () -> fileManager.saveImage(image, output),
                     "Should throw FileAlreadyExistsException when saving image to already existing file");
    }

    @Test
    void testSaveImageSuccessfullySavedImagePNG(@TempDir Path tempDir) throws IOException {
        Path filePath = tempDir.resolve("test.png");
        Files.createFile(filePath);

        File output = tempDir.resolve("test-new.png").toFile();
        fileManager.saveImage(image, output);

        assertTrue(output.exists(), "Should have successfully saved.png image");
    }

    @Test
    void testSaveImageSuccessfullySavedImageJPG(@TempDir Path tempDir) throws IOException {
        Path filePath = tempDir.resolve("test.jpg");
        Files.createFile(filePath);

        File output = tempDir.resolve("test-new.jpg").toFile();
        fileManager.saveImage(image, output);

        assertTrue(output.exists(), "Should have saved .jpg image");
    }

    @Test
    void testSaveImageSuccessfullySavedImageJPEG(@TempDir Path tempDir) throws IOException {
        Path filePath = tempDir.resolve("test.jpeg");
        Files.createFile(filePath);

        File output = tempDir.resolve("test-new.jpeg").toFile();
        fileManager.saveImage(image, output);

        assertTrue(output.exists(), "Should have saved .jpeg image");
    }

    @Test
    void testSaveImageSuccessfullySavedImageBMP(@TempDir Path tempDir) throws IOException {
        Path filePath = tempDir.resolve("test.bmp");
        Files.createFile(filePath);

        File output = tempDir.resolve("test-new.bmp").toFile();
        fileManager.saveImage(image, output);

        assertTrue(output.exists(), "Should have saved .bmp image");
    }

    @Test
    void testSaveImageSavingToUnsupportedFormatThrows(@TempDir Path tempDir) throws IOException {
        Path filePath = tempDir.resolve("test.bmp");
        Files.createFile(filePath);

        File output = tempDir.resolve("test-new.csv").toFile();
        assertThrows(IOException.class, () -> fileManager.saveImage(image, output),
                     "Should throw IOException when saving image to unsupported format");
    }

    @Test
    void testSaveImageSuccessfullySavedImageChildDir(@TempDir Path tempDir) throws IOException {
        Path parentDir = Files.createDirectory(tempDir.resolve("temp"));
        File output = parentDir.resolve("test.png").toFile();

        fileManager.saveImage(image, output);

        assertTrue(output.exists(), "Image should be saved in child directory");
    }

    @Test
    void testLoadImagesFromDirectoryNullDirectoryThrows() {
        assertThrows(IllegalArgumentException.class,
                     () -> fileManager.loadImagesFromDirectory(null),
                     "Should throw IllegalArgumentException when loading images from null directory");
    }

    @Test
    void testLoadImagesFromDirectoryNonExistingDirectoryThrows(@TempDir Path tempDir) {
        Path nonExistingDir = tempDir.resolve("missing");

        assertThrows(FileNotFoundException.class,
                     () -> fileManager.loadImagesFromDirectory(nonExistingDir.toFile()),
                     "Should throw FileNotFoundException when directory does not exist");
    }

    @Test
    void testLoadImagesFromDirectoryFileInsteadOfDirectoryThrows(@TempDir Path tempDir) throws IOException {
        Path regularFile = tempDir.resolve("not-a-dir.txt");
        Files.writeString(regularFile, "content");

        assertThrows(FileNotFoundException.class,
                     () -> fileManager.loadImagesFromDirectory(regularFile.toFile()),
                     "Should throw FileNotFoundException when given path is a file, not a directory");
    }

    @Test
    void testLoadImagesFromDirectoryEmptyDirectoryReturnsEmptyList(@TempDir Path tempDir) throws IOException {
        Path emptyDir = Files.createDirectory(tempDir.resolve("empty"));

        List<BufferedImage> images = fileManager.loadImagesFromDirectory(emptyDir.toFile());

        assertTrue(images.isEmpty(),
                   "Should return an empty list when the directory contains no files");
    }

    @Test
    void testLoadImagesFromDirectoryLoadsAllImages(@TempDir Path tempDir) throws IOException {
        Path imagesDir = Files.createDirectory(tempDir.resolve("images"));

        BufferedImage img1 = new BufferedImage(10, 10, BufferedImage.TYPE_INT_RGB);
        BufferedImage img2 = new BufferedImage(20, 20, BufferedImage.TYPE_INT_RGB);

        File imgFile1 = imagesDir.resolve("first.png").toFile();
        File imgFile2 = imagesDir.resolve("second.jpg").toFile();

        ImageIO.write(img1, "png", imgFile1);
        ImageIO.write(img2, "jpg", imgFile2);

        List<BufferedImage> loadedImages = fileManager.loadImagesFromDirectory(imagesDir.toFile());

        assertEquals(2, loadedImages.size(), "Should load all image files from the directory");
        assertTrue(loadedImages.stream().allMatch(Objects::nonNull),
                   "Loaded images should not contain null elements");
    }

    @Test
    void testLoadImagesFromDirectoryWithUnsupportedFileThrows(@TempDir Path tempDir) throws IOException {
        Path imagesDir = Files.createDirectory(tempDir.resolve("images"));

        BufferedImage img = new BufferedImage(10, 10, BufferedImage.TYPE_INT_RGB);
        File imgFile = imagesDir.resolve("valid.png").toFile();
        ImageIO.write(img, "png", imgFile);

        File unsupportedFile = imagesDir.resolve("not-image.txt").toFile();
        Files.writeString(unsupportedFile.toPath(), "not an image");

        assertThrows(IOException.class,
                     () -> fileManager.loadImagesFromDirectory(imagesDir.toFile()),
                     "Should throw IOException when directory contains a file with unsupported format");
    }

    @Test
    void testLoadImagesFromDirectoryWithNestedDirectories(@TempDir Path tempDir) throws IOException {
        Path imagesDir = Files.createDirectory(tempDir.resolve("images"));
        Files.createDirectory(imagesDir.resolve("nested"));

        BufferedImage img1 = new BufferedImage(10, 10, BufferedImage.TYPE_INT_RGB);
        File imgFile1 = imagesDir.resolve("first.png").toFile();
        ImageIO.write(img1, "png", imgFile1);

        List<BufferedImage> loadedImages = fileManager.loadImagesFromDirectory(imagesDir.toFile());

        assertEquals(1, loadedImages.size(), "Should exclude nested directories when loading images from directory");
        assertTrue(loadedImages.stream().allMatch(Objects::nonNull),
                   "Loaded images should not contain null elements");
    }
}
