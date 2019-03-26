---
title: Handy command-line incantations
author: Janne Hellsten
public: true
---

Just some command line copy&pastes that I always tend to forget.  Mainly ImageMagick and ffmpeg.

Image files
-----------

View image width, height & format:

```
identify <imagefile>
```

Make a .gif anim from a collection of image files:

```
convert -loop 0 -delay 300 image1.png image2.png image3.png result.gif
```

Resize an image with point-sampling while NOT preserving aspect-ratio (ImageMagick forces original aspect ratio without warning):

```
convert input.png -sample 640x200! output.png
```

Double image width and height with point-sampling:

```
convert in.png -filter point -resize 200% out.png
```

Stack two images side by side:

```
convert left.png right.png -gravity center +append out.png
```

Crop the input image and then make a .gif (`+repage` is required to actually crop the output image dimensions too):

```
# 104x30 is the size of the crop rectangle
# +62+22 is an offset to the top/left corner of the crop area
convert -crop 104x30+62+22 +repage -loop 0 -delay 300 image?.png result.gif
```

Video files
-----------

View video file format details:

```
ffprobe <videofile>
```

Convert a VICE video capture .avi (in DivX format) to MP4:

```
ffmpeg -i screencap.avi -c:v libx264 -crf 19 -preset slow -c:a libvo_aacenc -b:a 192k -ac 2 out.mp4
# or a simpler version (save to .avi the lossless FFV1 codec in VICE)
ffmpeg -i screencap.avi -c:v libx264 -pix_fmt yuv420p -preset veryslow -crf 5 foo.mp4
```

Convert a series of image files into an MP4:

```
ffmpeg -r 25 -i 'frame_%04d.png' -c:v libx264 -vf fps=25 -pix_fmt yuv420p out.mp4
```

Convert a MOV file into an MP4 (e.g., for uploading Quicktime screencaps to Twitter):

```
ffmpeg -i movie.mov -vcodec copy -acodec copy out.mp4
```

