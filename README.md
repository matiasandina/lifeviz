
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lifeviz

<!-- badges: start -->

<!-- badges: end -->

The goal of [lifeviz](https://matias-andina.shinyapps.io/lifeviz) is to
visualize life events. This has been inspired by the post [‘The Tail
End’ by WaitButWhy](https://waitbutwhy.com/2015/12/the-tail-end.html).
I have taken the visualizations and added a few degrees of freedom, so
that the users can visualize the past/present. On the activities tab,
users can display the remaining activity counts for things they could
represent using emojis.

The idea is to produce reflection and general introspection about our
time on Earth. Actually visualizing the number of remaining
days/burgers/hugs, can be a powerful tool to do so.

Check out the online version of the app
[here](https://matias-andina.shinyapps.io/lifeviz)\!

### Usage

The app contains two tabs. One of them will prompt for your date of
birth and the number of years lived you are considering on this Earth.

The second tab allows you to plot the number of remaining activities
using an emoji selector. You can customize the title of the display. If
the number of events/activities is too large, each emoji will represent
more than one event/activity. This was done to keep the dimensions of
the images manageable.

The example below shows how to use the basic functions.

![](example.gif)

Check out the online version of the app
[here](https://matias-andina.shinyapps.io/lifeviz)\!

### User data

This app does not store user data beyond the usage session nor it sends
information to the creator of the app.

### Contribute

Some of the functionality might be buggy. This is specially true about.
Week calculation might be a bit tricky around the end of the year (this
app uses a mix of calendar weeks and iso weeks).

The \`X years of life in weeks\` plot uses the calendar weeks and not
the actual date of birth as the beginning. This is a design decision
that introduces a small inaccuracy to produce what I believe is a more
useful visualization of the weeks in life (we mostly think of years
starting at January 1st regardless of our birth date).

If you find a bug or want to contribute, please file issues to improve
this app.

Hosting this app online is not for free. If you like what I do, consider
sponsoring my work.

### License

This was created by [Matias Andina](https://twitter.com/NeuroMLA). See
[LICENSE.md](https://github.com/matiasandina/lifeviz/blob/main/LICENSE.md).
