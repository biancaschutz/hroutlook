
  const cards = document.querySelectorAll(".intro-card")
 function updateCards() {
  const zoneCenter = window.innerHeight * 0.8;

  let activeCard = null;
  let closest = Infinity;

  cards.forEach(card => {
    const rect = card.getBoundingClientRect();
    const center = rect.top + rect.height / 2;
    const distance = Math.abs(center - zoneCenter);

    if (distance < closest) {
      closest = distance;
      activeCard = card;
    }
  });

  cards.forEach(card => {
    const active = card === activeCard;

    card.style.setProperty('--card-opacity', active ? 1 : 0);
    card.style.setProperty('--card-scale', active ? 1 : 0.9);
  });
}
window.addEventListener('scroll', updateCards, { passive: true });
window.addEventListener('resize', updateCards);
updateCards();

  let tocId = 'toc';

  let headings;
  let headingIds = [];
  let headingIntersectionData = {};
  let headerObserver;

  function setLinkActive(link) {
    const items = document.querySelectorAll(`#${tocId} li`);
    items.forEach((item) => item.classList.remove('active'));
    if (link) {
      link.closest('li').classList.add('active');
    }
  }

  function getProperListSection(heading, previousHeading, currentListElement) {
  let listSection = currentListElement;
  if (previousHeading) {
    if (heading.tagName.slice(-1) > previousHeading.tagName.slice(-1)) {
      let nextSection = document.createElement('ul');
      nextSection.classList.add('toc');   // <-- fixed: create <ul>, then add class
      listSection.appendChild(nextSection);
      return nextSection;
    } else if (heading.tagName.slice(-1) < previousHeading.tagName.slice(-1)) {
      let indentationDiff = parseInt(previousHeading.tagName.slice(-1)) - parseInt(heading.tagName.slice(-1));
      while (indentationDiff > 0) {
        listSection = listSection.parentElement;
        indentationDiff--;
      }
    }
  }
  return listSection;
}

  function setIdFromContent(element, appendedId) {
    if (!element.id) {
      let slug = element.innerHTML
        .replace(/<[^>]*>/g, '')
        .trim()
        .toLowerCase()
        .replace(/[^a-z0-9\s-]/g, '')
        .replace(/\s+/g, '-');
      if (/^[0-9]/.test(slug)) {
        slug = `h-${slug}`;
      }
      element.id = `${slug}-${appendedId}`;
    }
  }

  function addNavigationLinkForHeading(heading, currentSectionList) {
    let listItem = document.createElement('li');
    let anchor = document.createElement('a');
    anchor.innerHTML = heading.innerHTML;
    anchor.id = `${heading.id}-link`;
    anchor.href = `#${heading.id}`;
    anchor.onclick = (e) => {
      setTimeout(() => {
        setLinkActive(anchor);
      });
    };
    listItem.appendChild(anchor);
    currentSectionList.appendChild(listItem);
  }

  function buildTableOfContentsFromHeadings() {
  const tocElement = document.querySelector(`#${tocId}`);
  const main = document.querySelector('.story-content');
  if (!main) {
    throw Error('A `main` tag section is required to query headings from.');
  }
  tocElement.innerHTML = '';

  headings = main.querySelectorAll('h2, h3, h4, h5, h6');
  let previousHeading;
  let currentSectionList = document.createElement('ul');
  currentSectionList.classList.add('toc');   // <-- add class here
  tocElement.appendChild(currentSectionList);

  headings.forEach((heading, index) => {
    currentSectionList = getProperListSection(heading, previousHeading, currentSectionList);
    setIdFromContent(heading, index);
    addNavigationLinkForHeading(heading, currentSectionList);

    headingIds.push(heading.id);
    headingIntersectionData[heading.id] = { y: 0 };
    previousHeading = heading;
  });
}

  function updateActiveHeadingOnIntersection(entry) {
    const previousY = headingIntersectionData[entry.target.id].y;
    const currentY = entry.boundingClientRect.y;
    const id = `#${entry.target.id}`;
    const link = document.querySelector(id + '-link');
    const index = headingIds.indexOf(entry.target.id);

    if (entry.isIntersecting) {
      if (currentY > previousY && index !== 0) {
        const prevLink = document.querySelector(`#${headingIds[index - 1]}-link`);
        setLinkActive(prevLink);
      } else {
        setLinkActive(link);
      }
    } else {
      if (currentY > previousY && index > 0) {
        const lastLink = document.querySelector(`#${headingIds[index - 1]}-link`);
        setLinkActive(lastLink);
      }
    }

    headingIntersectionData[entry.target.id].y = currentY;
  }

  function observeHeadings() {
    let options = {
      root: document.querySelector('main'),
      threshold: 0.1,
    };
    headerObserver = new IntersectionObserver((entries) => entries.forEach(updateActiveHeadingOnIntersection), options);
    Array.from(headings)
      .reverse()
      .forEach((heading) => headerObserver.observe(heading));
  }

  window.addEventListener('load', (event) => {
    buildTableOfContentsFromHeadings();
    if ('IntersectionObserver' in window) {
      observeHeadings();
    }
  });