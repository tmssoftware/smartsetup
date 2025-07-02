(function () {
  var lunr = require('lunr')

  var lunrIndex;

  var stopWords = null;
  var searchData = {};

  lunr.tokenizer.separator = /[\s\-\.\(\)]+/;

  const fs = require('fs');
  fs.readFile('search-stopwords.json', 'utf8', (err, data) => {
    if (err) throw err;
    stopWords = JSON.parse(data);
    buildIndex();
  });
  

  fs.readFile('index.json', 'utf8', (err, data) => {
    if (err) throw err;
    searchData = JSON.parse(data);
    buildIndex();
  });
  

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
    if (stopWords !== null && !isEmpty(searchData)) {
      lunrIndex = lunr(function () {
        this.pipeline.remove(lunr.stopWordFilter);
        this.ref('href');
        this.field('title', { boost: 50 });
        this.field('keywords', { boost: 20 });

        for (var prop in searchData) {
          if (searchData.hasOwnProperty(prop)) {
            this.add(searchData[prop]);
          }
        }

        //var docfxStopWordFilter = lunr.generateStopWordFilter(stopWords);
        //lunr.Pipeline.registerFunction(docfxStopWordFilter, 'docfxStopWordFilter');
        //this.pipeline.add(docfxStopWordFilter);
        //this.searchPipeline.add(docfxStopWordFilter);
      });

      fs.writeFileSync('compiled_idx.json', JSON.stringify(lunrIndex), 'utf8')
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
