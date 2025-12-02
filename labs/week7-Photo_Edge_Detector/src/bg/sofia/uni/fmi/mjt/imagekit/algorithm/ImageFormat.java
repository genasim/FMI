package bg.sofia.uni.fmi.mjt.imagekit.algorithm;

public enum ImageFormat {
    JPEG(".jpeg"), PNG(".png"), BMP(".bmp");

    private final String extension;

    ImageFormat(String extension) {
        this.extension = extension;
    }

    public String getExtension() {
        return extension;
    }
}
