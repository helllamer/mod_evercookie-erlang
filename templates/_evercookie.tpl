<script type="text/javascript">
  
  var _cookievalue = '{{ m.evercookie.new }}';
  var _cookiename = '{{ m.evercookie.name.png }}';
  
  var _evercookie_png = '{% url evercookie_png %}';
  var _evercookie_cache = '{% url evercookie_cache %}';
  var _evercookie_etag = '{% url evercookie_etag %}';
  
  var ec = new evercookie();
  
  ec.set(_cookiename, _cookievalue);
  
  ec.get("uid", function(best, all) {
    
    console.log( 'best: ' + best );
    
    for (var item in all)
      console.log( all[item] );
    
  });
  
</script>
