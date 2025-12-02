package bg.sofia.uni.fmi.mjt.imagekit.filesystem;

import bg.sofia.uni.fmi.mjt.imagekit.algorithm.ImageFormat;
import bg.sofia.uni.fmi.mjt.imagekit.cli.exceptions.IllegalImageFormatException;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.FileAlreadyExistsException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

public class LocalFileSystemImageManager implements FileSystemImageManager {
    @Override
    public BufferedImage loadImage(File imageFile) throws IOException {
        verifyLoadedFilePath(imageFile);
        return ImageIO.read(imageFile);
    }

    @Override
    public List<BufferedImage> loadImagesFromDirectory(File imagesDirectory) throws IOException {
        verifyDirectoryPath(imagesDirectory);

        List<File> childFiles = new ArrayList<>();
        for (File file : Objects.requireNonNull(imagesDirectory.listFiles())) {
            if (file.isFile()) {
                childFiles.add(file);
            }
        }

        if (childFiles.isEmpty()) {
            return List.of();
        }

        List<BufferedImage> images = new ArrayList<>();
        for (File file : childFiles) {
            images.add(loadImage(file));
        }

        return images;
    }

    @Override
    public void saveImage(BufferedImage image, File imageFile) throws IOException {
        if (image == null) {
            throw new IllegalArgumentException("Cannot save null image");
        }
        verifySavedFilePath(imageFile);

        try {
            ImageFormat format = getFormat(imageFile);
            ImageIO.write(image, format.getExtension(), imageFile);
        } catch (IllegalImageFormatException e) {
            throw new IOException(e.getMessage(), e.getCause());
        }
    }

    private ImageFormat getFormat(File imageFile) {
        String name = imageFile.getName();
        int lastDotIndex = name.lastIndexOf(".");
        String extension = name.substring(lastDotIndex + 1);

        return switch (extension) {
            case "jpeg" -> ImageFormat.JPEG;
            case "png" -> ImageFormat.PNG;
            case "bmp" -> ImageFormat.BMP;
            default -> throw new IllegalImageFormatException(extension);
        };
    }

    private boolean isSupportedFormat(File imageFile) {
        String fileName = imageFile.getName();
        for (ImageFormat format : ImageFormat.values()) {
            if (fileName.endsWith(format.getExtension())) {
                return true;
            }
        }
        return false;
    }

    private void verifyLoadedFilePath(File filePath) throws IOException {
        if (filePath == null) {
            throw new IllegalArgumentException("Cannot load null image");
        }

        if (!filePath.isFile()) {
            throw new IOException("File at " + filePath.getAbsolutePath() + " is not a file");
        }

        if (!isSupportedFormat(filePath)) {
            throw new IOException(
                "File at " + filePath.getAbsolutePath() + " is not one of the supported formats: " +
                    Arrays.toString(ImageFormat.values()));
        }
    }

    private void verifySavedFilePath(File filePath) throws FileNotFoundException, FileAlreadyExistsException {
        if (filePath == null) {
            throw new IllegalArgumentException("Cannot save image to null file");
        }

        if (!filePath.getParentFile().exists()) {
            throw new FileNotFoundException("Parent directory of " + filePath.getAbsolutePath() + " does not exist");
        }

        if (filePath.exists()) {
            throw new FileAlreadyExistsException(filePath.getAbsolutePath());
        }
    }

    private void verifyDirectoryPath(File directory) throws FileNotFoundException {
        if (directory == null) {
            throw new IllegalArgumentException("Cannot save image to null directory");
        }

        if (!directory.exists()) {
            throw new FileNotFoundException("Directory " + directory.getAbsolutePath() + " does not exist");
        }

        if (!directory.isDirectory()) {
            throw new FileNotFoundException("File " + directory.getAbsolutePath() + " is not a directory");
        }
    }
}
