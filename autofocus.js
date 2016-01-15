new MutationObserver(function(mutations) {
  mutations.forEach(function(mutation) {
    var node = mutation.target;
    if (node instanceof Element && node.hasAttribute('autofocus')) {
      node.focus();
    }
  });
})
.observe(document.querySelector('body > div'), { childList: true, subtree: true, attributes: true, attributeFilter: ['autofocus'] });