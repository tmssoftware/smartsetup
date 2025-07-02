(function () {
  importScripts('lunr.min.js');

  var lunrIndex;

  var searchData = {};
  var indexData = {};

  var searchDataRequest = new XMLHttpRequest();

  searchDataRequest.open('GET', '../index.json');
  searchDataRequest.onload = function () {
    if (this.status != 200) {
      return;
    }
    searchData = JSON.parse(this.responseText);

    buildIndex();
  }
  searchDataRequest.send();

  var indexDataRequest = new XMLHttpRequest();

  indexDataRequest.open('GET', '../compiled_idx.json');
  indexDataRequest.onload = function () {
    if (this.status != 200) {
      return;
    }
    indexData = JSON.parse(this.responseText);

    buildIndex();

    postMessage({ e: 'index-ready' });
  }
  indexDataRequest.send();


  onmessage = function (oEvent) {
    var q = oEvent.data.q;
    var hits = lunrIndex.search(q);
    var results = [];
    hits.forEach(function (hit) {
      var item = searchData[hit.ref];
      results.push({ 'href': item.href, 'title': item.title, 'keywords': item.keywords });
    });
    postMessage({ e: 'query-ready', q: q, d: results });
  }

  function buildIndex() {
    if (!isEmpty(searchData) && !isEmpty(indexData)) {
      lunrIndex = lunr.Index.load(indexData)
    }
  }
  

  function isEmpty(obj) {
    if(!obj) return true;

    for (var prop in obj) {
      if (obj.hasOwnProperty(prop))
        return false;
    }

    return true;
  }
})();
