---
title: An ode to reveal.js
date: 2025-10-23T00:00:00.000Z
description: An ode to reveal.js
slug: ode-to-revealjs
categories:
  - miscellaneous
tags:
  - miscellaneous
  - revealjs
engine: knitr
editor_options:
  chunk_output_type: console
---


<style type="text/css">
.iframe-container {
  width: 100%;
  height: 420px;
  overflow: hidden;
}

.iframe-container.large {
  height: 600px;
}

.iframe-container iframe {
  transform-origin: top left;
  transform: scale(calc(100% / 1920px * 900px));
}

@media (max-width: 900px) {
  .iframe-container {
    height: 200px;
  }

  .iframe-container.large {
    height: 260px;
    width: 120%;
    transform-origin: top left;
    transform: scale(calc(85%));
    overflow: hidden;
  }

  .iframe-container iframe {
    transform: scale(calc(100% / 1920px * 420px));
  }
}
</style>

[Reveal.js](https://revealjs.com) is awesome and in this post I'll explain why. If you have never heard of it, reveal.js is an open source tool to create presentations (slide decks if you prefer). While it does have a [GUI wrapper](http://slides.com) by the same developer [Hakim El Hattab](https://hakim.se) ([sponsor link](https://github.com/sponsors/hakimel?frequency=one-time)), I have been using the HTML/CSS framework to build mine, and I love it. Before I dive I'll have to mention that I know it's not for everyone, but for people that have done a bit of web design, reveal.js may be the last presentation builder you'll ever use. In this post I'll describe why.

> **TLDR;** [reveal.js](https://revealjs.com) is an open source, HTML/CSS/JS based presentation builder with an active community developing plugins and integrations that allows developers with some web designer experience to create responsive, creative, flexible, and beautiful slides for presentations that are easy to share.

I was in the market for a new presentation builder after my continued frustration with PowerPoint reached a boiling point{{< sidenote >}}This was even before they inserted the Copilot AI stuff, it was terrible for design before{{< /sidenote >}}. I had just gotten experienced with [LaTeX](https://www.latex-project.org) and the advantages of working in a WYMIWYG{{< sidenote >}}*What You Mean Is What You Get*, like LaTeX, as opposed to WYSIWYG (*What You See Is What You Get*), like Word or Powerpoint{{< /sidenote >}} framework and I didn't like the ["beamer"](https://www.overleaf.com/learn/latex/Beamer) option in LaTeX, I wanted more control over the design and deploy modern design principles. This is where I found reveal.js and it has been my main tool for presentation design outside my corporate job (where I still have to use Powerpoint for good reasons).

If you have never seen a reveal.js presentation, here is the [official demo](https://revealjs.com/demo/) highlighting some of the built-in functionality (if you're on mobile, the slides below all support [touch navigation](https://revealjs.com/touch-navigation/)):

{{< revealjs src=\"https://revealjs.com/demo/\" style=\"width: 1600px; height: 800px;\" >}}

{{< details summary=\"Simple installation instructions\" >}}
Installation of reveal.js is quite simple. The [guide](https://revealjs.com/installation/) recommends using [Node.js](https://nodejs.org/) but I think it's easier to just clone the repository and work from there. It requires no further setup, just open the `index.html` file and you're good to go. Any changes can be made by editing the `index.html` file or add or edit stylesheets and scripts.

``` bash
git clone https://github.com/hakimel/reveal.js.git presentation-slides
cd presentation-slides
open index.html # supposed to open the slides in your browser
```

{{< /details >}}

Why do I care so much about design in a presentation builder? Particularly for academics, presenting for a seminar or a conference is possibly the only time others really interact with your personal expertise. Case in point, I have posts on this website that are cited more than some of my published articles in prestige journals. This means that your only chance to educate others on your topic of interest is through presentations. And for students who have to sit through a lot of dense seminars each week, or conference attendees that are seeing a bunch of presentations over the course of a few days, making sure your presentations follow good design principles make sure you get your message across more effectively and with the least amount of friction. Reveal.js allows me to do this more effectively than PowerPoint ever did, both in the creation stage as the presentation stage.

Here are some reasons why:

## Efficiency

While I believe even the defaults in reveal.js are better design-wise than the defaults in PowerPoint{{< sidenote >}}I compare it here to PowerPoint, but the same goes for LibreOffice's Impress, and for Apple's Keynote to a lesser degree{{< /sidenote >}}, reveal.js lets me design the presentation the way I want to{{< sidenote >}}call me a nerd but I like writing code{{< /sidenote >}}, while a GUI-based system forces me to design the way (for example) Microsoft or Apple wants me to. A frictionless workflow lets me work efficiently and focus on improving the content of the presentation and pedagogical challenges instead of the technicalities of the tool I'm using. Particularly if you teach statistics, mathematics, or programming, the efficiency of typing formulas in LaTeX sytax and letting [KaTeX](https://katex.org) or [MathJax](https://www.mathjax.org) handle the styling ([docs](https://revealjs.com/math/)), or typing code letting [highlight.js](https://highlightjs.org) do its magic ([docs](https://revealjs.com/code/)), is pure bliss.

If you're not used to writing HTML, CSS, or Javascript code, the learning curve is very steep. Once you're comfortable with these frameworks the barriers very quickly dissappear and I found reveal.js quite intuitive. Separating the content from the style makes it not just efficient when designing individual slide decks, it also makes transferring the same content to new slide decks very simple since you can copy the HTML code and the CSS of the new slide deck will take care of the rest. This also applies for screen sizes, since you don't need to specify an aspect ratio beforehand (e.g. 16x9 or 4x3), you save having to adjust your presentations for different projectors and screens since (like any webpage with good responsive design) it would just adjust for the screen size accordingly. The [slide deck below](https://slides.danielroelfs.app/2023-10-26-psi-phd-seminar/psi-phd-seminar-slides/index.html) is just another example of a slideshow I used for a workshop:

{{< revealjs src=\"https://slides.danielroelfs.app/2023-10-26-psi-phd-seminar/psi-phd-seminar-slides/index.html\" style=\"width: 1600px; height: 900px;\" >}}

## Shareability

A reveal.js presentation is essentially just an HTML website. It can be shared like any other website, for example on GitHub Pages. This makes it quite a bit easier to share presentations, since one doesn't need to own a particular software to view the presentation. This makes it exceedingly easy to share the presentation without having to export it to PDF (and lose all transitions, layered content on your slide overlapping, etc.). You can open source your presentations and allow others to learn and (re)use your work. This has allowed me to share almost all slide decks (that weren't confidential) on [this website](https://slides.danielroelfs.app) (best viewed on a laptop or desktop, [source code here](http://github.com/danielroelfs/slides)). This shareability is a major advantage for allowing students and others to revisit the slides in an easy matter that doesn't require them to download software or keep track of large PDF files (though [exporting reveal.js presentations to PDF](https://revealjs.com/pdf-export/) is of course still possible)

## Integrations

I use [Quarto](https://quarto.org) quite a lot, including to generate the Markdown files that Hugo then uses to build this site. Quarto has quickly become one of my favorite tools, and because it's open source as well, the integration possibilities are limited only by the creativity of the developers. This is why there is also [out-of-the-box integration with reveal.js](https://quarto.org/docs/presentations/revealjs/). Especially for teaching programming workshops this is extremely useful{{< sidenote >}}Particularly when you use the [reveal.js functionality to present code incrementally](https://revealjs.com/code/){{< /sidenote >}}, since you can still bring your own CSS files to style the presentation, but all code and the output from the code is generated from this single Quarto document that then generates the reveal.js presentation. A simple Quarto presentation can be as simple as this:

``` bash
---
title: "Example slides"
author: "J. Doe"
format:
  revealjs:
    transition: slide
    controls: true
    controls-layout: bottom-right
---

## Slide 1

- Item 1
- Item 2

---

![The image caption](image.png){fig-alt="Alterate description for the image"}
```

For a more elaborate example, the slide deck below is created entirely in Quarto with the `revealjs` format (source code [here](https://github.com/danielroelfs/slides/blob/main/2023-10-26-psi-phd-seminar/psi-phd-seminar-slides/index.qmd)). I also found the [online book *Slidecrafting*](https://slidecrafting-book.com) by [Emil Hvitfeldt](https://emilhvitfeldt.com) to be an excellent resource on creating reveal.js slides through Quarto.

{{< revealjs src=\"https://quarto.org/docs/presentations/revealjs/demo/\" style=\"width: 1600px; height: 800px;\" >}}

In addition to Quarto, the number of plugins that offer various functionality is essentially endless. The main relevant one for laymen might be the option to create slides not using HTML, but using [Markdown](https://revealjs.com/markdown/) (something also Quarto uses) that makes the creation step quite a bit easier for those without experience with HTML.

## Templating

The WYMIWYG method of separating the content from the style makes it exceptionally easy to create nice and consistent-looking templates. While I know templates work very well for PowerPoint as well, individual template slides are hard to adjust. With reveal.js the styling allows for a lot of adjustments while maintaining an overall brand consistency. Generally, if you'd like some inspiration for what is possible with reveal.js I'd recommend to search around on GitHub for others who have used it. Particularly the R community has embraced reveal.js through the Quarto implementation and there are a number of examples on the [conference materials repo](https://github.com/posit-conf-2025) for [Posit::conf](https://posit.co/conference/). Reveal.js examples and templates using the straight HTML implementation can be found on GitHub in abundance highlighting different use cases and scenarios. For myself I adapted a [template from someone else](https://gapple.github.io/myplanet-revealjs/) I found on GitHub for use within my research center:

{{< revealjs src=\"https://danielroelfs.github.io/revealjs-norment-theme/\" style=\"width: 1920px; height: 1200px;\" parent_class=\"large\" >}}

## Reliability

More and more cyber security experts are worried about the rapid slide of the US government towards authoritarianism, and a large number of European organisations and companies are moving towards alternatives not reliant on US software and hardware such as [LibreOffice](https://www.libreoffice.org) as in the [case of a German *Bundesland*](https://www.zdnet.com/article/german-state-ditches-microsoft-for-linux-and-libreoffice/) and the [Austrian army](https://www.zdnet.com/article/this-european-military-just-ditched-microsoft-for-open-source-libreoffice-heres-why/).

Open source alternatives are often the most attractive alternatives due to their relative low cost, degree of control over privacy and security matters, and that any bugs can be quickly implemented by any contributor{{< sidenote >}}Security flaws can also be quickly identified by bad actors too, it should be noted{{< /sidenote >}}. Using an open source tool also ensures the independence of your project. You don't need to buy and install proprietary software, there is transparency about what happens with the source code, and there are no licenses involved that require periodic internet access.

## Creativity

When I am making a PowerPoint I rarely (if ever) feel inspired, a feeling I'm sure many can relate to about any Microsoft product. However, due to the community around reveal.js, the overwhelming amount of creatives that write and share their HTML/CSS/Javascript code with others through websites like [CodePen.io](https://codepen.io/) or just [StackOverflow](https://stackoverflow.com/tags/reveal.js/). I'd even trust generic LLMs to generate more reliable code than Copilot would for PowerPoint. Point being that reveal.js allows me to be creative, while any creativity in PowerPoint is immediately stifled by having to refer to the documentation for custom transitions and having to click around *a lot* to get it done.

Also during presentations, the unique (afaik) feature of allowing both horizontal and vertical slides allows you to add extra content if you feel the audience would like more elaboration on certain topics that you could include in vertical slides, but otherwise move over by continuing horzontally. I've used it also for illustrative purposes to group related slides together and showing a "flow" of related concepts. I've used it a fair bit in the [slides for my PhD defense](https://slides.danielroelfs.app/2024-11-19-phd-defense/phd-defense_index.html) to allow faster scrolling back and forth for both the highlights during the intro, and all main figures and tables at the end:

{{< revealjs src=\"https://slides.danielroelfs.app/2024-11-19-phd-defense/disputation-slides/slides.html\" style=\"width: 1920px; height: 1200px;\" parent_class=\"large\" >}}

## Conclusion

I've always enjoyed teaching, but since I discovered reveal.js and started using it for all (major) presentations, the quality of my presentations has increased markedly. I use reveal.js whenever I get away with it, particularly for technical and mathematics heavy presentations. The learning curve is somewhat steep, but I've now gotten to the point where I can make a beautiful slide deck faster than I can make an equivalent PowerPoint presentation. I'll repeat that I acknowledge that reveal.js is not for everyone, and I'll believe anyone who says that most of these things can also be achieved by the seasoned PowerPoint user, but for those that enjoy doing some web design-y stuff, this tool is absolute bliss. It turns the presentation creation stage into a fun web design project and with minimal effort the presentations will look beautiful and modern without any of the PowerPoint aftertaste. I can recommend anyone looking for a great way to create beautiful and flexible presentations efficiently in VSCode to check out [reveal.js](https://revealjs.com).
