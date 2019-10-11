function uuidv4() {
  return ([1e7]+-1e3+-4e3+-8e3+-1e11).replace(/[018]/g, c =>
    (c ^ crypto.getRandomValues(new Uint8Array(1))[0] & 15 >> c / 4).toString(16)
  );
}

/* https://stackoverflow.com/a/21125098/945039 */
function getCookie(name) {
  var match = document.cookie.match(new RegExp('(^| )' + name + '=([^;]+)'));
  if (match) return match[2];
}

(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m);
})(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

if( getCookie('shinyid') === undefined ){
  document.cookie = 'shinyid='+uuidv4()+'; expires=Fri, 01 Jan 2100 12:00:00 UTC';
}
shinyid = getCookie('shinyid');

ga('create', 'UA-149767044-1', 'auto');
ga('send', 'pageview');

$(document).on('shiny:inputchanged', function(event) {
     ga('send','event',shinyid + '_' + new Date().getTime(),
     event.name, event.value, 'xxx');
  });