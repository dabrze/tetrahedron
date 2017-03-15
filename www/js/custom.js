$(document).ready(function(){
	$("#snapshot").click(function() {
		var rglel = $("#rglPlot");
		if (rglel.length===0){
			alert("bad rglwidgetId:"+ data.rglwidgetId);
			return null;
		}
		var rglinst = rglel[0].rglinstance;
		
		var fileName = "";
		
		if ($('#customMeasure').val()) {
			fileName = "custom_measure";
		} else {
			fileName = $('#measure').val();
		}
		
		var link = document.createElement("a");
		link.download = fileName + "_" + new Date().toJSON().slice(0,19) + ".png";
		link.href = rglinst.canvas.toDataURL("image/png");
		document.body.appendChild(link);
		link.click();
		document.body.removeChild(link);
		delete link;
	});
	
	$('#customMeasure').on('input', function(e){
		if ($('#customMeasure').val()) {
			$('#measure')[0].selectize.disable();
		} else {
			$('#measure')[0].selectize.enable();
		}
  });
  
  $(window).resize(function() {
    $("#hoverInfo").css('margin-left', $("#crossSectionPlot").offset().left - $(".hover-plot-wrapper").offset().left + 'px');
  });
  
  $("a[data-value='Cross-sections']").click(function() {
    setTimeout(function(){
      $("#hoverInfo").css('margin-left', 
                        $("#crossSectionPlot").offset().left - $(".hover-plot-wrapper").offset().left + 'px');},
    300);
  });
  
  var rglObserver = new MutationObserver(function(mutations) {
    if ($("html").hasClass("shiny-busy") || $("#rglPlot > canvas")[0] === undefined) {
      setTimeout(function(){
        if ($("html").hasClass("shiny-busy") || $("#rglPlot > canvas")[0] === undefined) {
          $("#rgl-spinner").show();
        }
      }, 1200);
    } else {
      $("#rgl-spinner").hide();
    }
  });
  var rglTarget = document.querySelector('html');
  rglObserver.observe(rglTarget, {
    attributes: true
  });
  
  var crossSectionObserver = new MutationObserver(function(mutations) {
    if ($("#crossSectionPlot").hasClass("recalculating") || $("#crossSectionPlot > img")[0] === undefined) {
      setTimeout(function(){
        if ($("#crossSectionPlot").hasClass("recalculating") || $("#crossSectionPlot > img")[0] === undefined) {
          $("#cross-section-spinner").show();
        }
      }, 1200);
    } else {
      $("#cross-section-spinner").hide();
    }
  });
  var crossSectionTarget = document.querySelector('#crossSectionPlot');
  crossSectionObserver.observe(crossSectionTarget, {
    attributes: true
  });
  
  if (!window.WebGLRenderingContext) {
     $("#rglPlot").parent().prepend("<div class='alert alert-warning alert-dismissible' role='alert'><button type='button' class='close' data-dismiss='alert' aria-label='Close'><span aria-hidden='true'>&times;</span></button>Your browser doesn't seem to support WebGL. Try another browser or check if you have WebGL support at: <a href='http://get.webgl.org'>http://get.webgl.org</a>.</div>");
     return;
  }

setTimeout(function(){
  try { gl = $("#rglPlot > canvas")[0].getContext("webgl"); }
  catch (x) { gl = null; }
  
  if (gl === null) {
      try { gl = $("#rglPlot > canvas")[0].getContext("experimental-webgl"); experimental = true; }
      catch (x) { gl = null; }
  }
  
  if (!gl) {
    $("#rglPlot").parent().prepend("<div class='alert alert-warning alert-dismissible' role='alert'><button type='button' class='close' data-dismiss='alert' aria-label='Close'><span aria-hidden='true'>&times;</span></button>Hmm... It's taking some time to initialize WebGL. Try refreshing the page, and if that doesn't help check if you have the latest drivers and browser: <a href='http://get.webgl.org/troubleshooting'>http://get.webgl.org/troubleshooting</a>.</div>");
    return;
  }}, 5000);
});

$(window).scroll(function() {
    if ($(this).scrollTop() >= 200) {        // If page is scrolled more than 50px
        $('.return-to-top').fadeIn(200);    // Fade in the arrow
    } else {
        $('.return-to-top').fadeOut(200);   // Else fade out the arrow
    }
});