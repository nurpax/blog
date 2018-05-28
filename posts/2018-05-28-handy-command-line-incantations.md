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

Video files
-----------

View video file format details:

```
ffprobe <videofile>
```

Convert a VICE video capture .avi (in DivX format) to MP4:

```
ffmpeg -i screencap.avi -c:v libx264 -crf 19 -preset slow -c:a libvo_aacenc -b:a 192k -ac 2 out.mp4
```
