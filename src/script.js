import enterView from 'enter-view';
import textBalancer from 'text-balancer';
import { select as d3Select } from 'd3-selection';
import 'd3-jetpack/essentials';

import { USE_COVER_HED } from '../config.yml';

/* Fade in navbar at scroll trigger */

const navbar = document.getElementById('navbar');
enterView({
  selector: USE_COVER_HED ? '.headline' : '.step-deck',
  offset: USE_COVER_HED ? 0.66 : 0.957,
  enter: () => {
    navbar.classList.remove('only-logo');
  },
  exit: () => {    
    navbar.classList.remove('show-nav-links');
    navbar.classList.add('only-logo');
  },
});

/* Mobile navbar hamburger trigger */

export function hamburgerTrigger() {
  navbar.classList.toggle('show-nav-links');
}

/* Text balance headline and deck */

textBalancer.balanceText('.headline, .deck, .image-overlay .image-caption');

/* Scatter plot */

const scatter = d3Select('#scatter');
scatter
  .append('div')
  .st({
    height: 100,
    margin: '0 auto',
    width: '90vw',
    border: '2px dotted red',
  });
console.log(scatter);
