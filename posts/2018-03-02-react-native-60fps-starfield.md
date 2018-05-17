---
title: 3D Starfield in React Native
author: Janne Hellsten
date: March 12, 2018
public: true
---

I've been working on an iOS port of my web game [Bintris][bintris].  I ported the HTML/Javascript version to React Native (RN) to make it run on iOS.

One of the game's main visual ingredients is the real-time 3D starfield background.  I originally put it in as a kind of placeholder but came to like it over time.  So I had to figure out a performant way to implement it on RN.

The web Bintris uses a pure-CSS starfield effect that I borrowed from [here][css3stars].  Here's how my RN re-implementation looks like:

<div class="youtube">
<iframe class="video" src="https://www.youtube.com/embed/thBijc6x7e0?rel=0&amp;showinfo=0" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>
</div>

Both the CSS and RN versions use a trick to project the starfield random 3d-points onto a fixed number of xy-planes.  This makes it quite a bit faster to animate and render than animating each 3D point separately.  In the above video, the starfield consists of ten Animated.Views each containing 150 random stars.  Each star is an absolutely positioned view with a white background color.

React renders the starfield only once upon mounting.  All further frame updates are done purely using RN animation with the native animation driver enabled using `useNativeDriver:true`.  This moves starfield rendering completely off the main JavaScript UI thread.  This results in buttery smooth animation and frees up the UI thread from processing all the 1500 views that make up the individual stars.

If you want to see it running on your phone, install the Expo app on your phone and use it to scan the QR code from [this Expo Snack](https://snack.expo.io/@nurpax/starfield).  The full source code to the starfield is contained within the Snack project.

## Improvements ideas

Here are some thoughts on what could still be improved:

* `shouldRasterizerIOS` should probably be used for any views rendered on top of the starfield.  Should you have any views that are rendered on top of the starfield, RN will render them at 60 FPS.  If the views are complex, you may run into performance or battery consumption problems.  The `shouldRasterizeIOS` should help by first rendering the static views into an image and then compositing the images on top using alpha blending.  Unfortunately, I didn't find enough information on how exactly the RN and/or iOS view renderer works, so I didn't enable this for my menus.

* The starfield would look smoother and less aliased if, instead of a white rectangle, a circle shaped image was used to render each individual star.

* RN stylesheet documentation recommends using `StyleSheet.create()` instead of inline styles.  I'm now using inline styles everywhere.  There may be a performance benefit to using `StyleSheet.create()`.  In my case though, each individual star needs its own stylesheet as a star's absolute position must be set using a stylesheet.

Thoughts?  Questions?  Let's discuss on [Reddit](https://www.reddit.com/r/reactjs/comments/83yrad/60_fps_3d_starfield_in_react_native/).

[bintris]: http://nurpax.com/bintris
[css3stars]: https://codepen.io/keithclark/pen/ibEnk
[animated]: https://facebook.github.io/react-native/docs/animated.html
