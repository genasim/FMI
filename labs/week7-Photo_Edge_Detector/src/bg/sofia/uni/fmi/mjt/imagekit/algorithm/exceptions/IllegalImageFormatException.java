package bg.sofia.uni.fmi.mjt.imagekit.algorithm.exceptions;

import bg.sofia.uni.fmi.mjt.imagekit.algorithm.ImageFormat;

import java.util.Arrays;

public class IllegalImageFormatException extends RuntimeException {
    public IllegalImageFormatException(String imageFormat) {
        super("Invalid image format: " + imageFormat + " Supported formats: " + Arrays.toString(ImageFormat.values()));
    }

    public IllegalImageFormatException(String message, Throwable cause) {
    }

    public ImageFormat[] supportedFormats() {
        return ImageFormat.values();
    }
}
