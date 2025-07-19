
// www/custom.js - Custom JavaScript for RTV Dashboard

// Initialize when document is ready
$(document).ready(function() {
  
  // Add smooth scrolling
  $('a[href*="#"]').on('click', function(e) {
    e.preventDefault();
    $('html, body').animate({
      scrollTop: $($(this).attr('href')).offset().top
    }, 500, 'linear');
  });
  
  // Add number counter animation for value boxes
  $('.small-box h3').each(function() {
    var $this = $(this);
    var countTo = $this.text();
    
    // Check if it's a number
    if (!isNaN(countTo.replace(/[,%]/g, ''))) {
      $this.prop('Counter', 0).animate({
        Counter: parseInt(countTo.replace(/[,%]/g, ''))
      }, {
        duration: 2000,
        easing: 'swing',
        step: function(now) {
          $this.text(Math.ceil(now).toLocaleString());
        }
      });
    }
  });
  
  // Enhanced search functionality
  Shiny.addCustomMessageHandler('highlightSearch', function(message) {
    var searchTerm = message.term;
    var selector = message.selector || 'body';
    
    // Remove previous highlights
    $(selector).unhighlight();
    
    // Add new highlights
    if (searchTerm && searchTerm.length > 2) {
      $(selector).highlight(searchTerm);
    }
  });
  
  // Add loading overlay for long operations
  Shiny.addCustomMessageHandler('showLoading', function(message) {
    var html = '<div class="loading-overlay">' +
               '<div class="loading-spinner"></div>' +
               '<div class="loading-text">' + (message.text || 'Loading...') + '</div>' +
               '</div>';
    $('body').append(html);
  });
  
  Shiny.addCustomMessageHandler('hideLoading', function(message) {
    $('.loading-overlay').fadeOut(300, function() {
      $(this).remove();
    });
  });
  
  // Optimize Plotly rendering
  window.optimizePlotly = function(plotId) {
    var plot = document.getElementById(plotId);
    if (plot && plot.data) {
      Plotly.relayout(plot, {
        'xaxis.fixedrange': true,
        'yaxis.fixedrange': true,
        'showTips': false,
        'doubleClick': false,
        'showAxisDragHandles': false
      });
    }
  };
  
  // Add keyboard shortcuts
  $(document).keydown(function(e) {
    // Ctrl/Cmd + K for search focus
    if ((e.ctrlKey || e.metaKey) && e.keyCode === 75) {
      e.preventDefault();
      $('#search_global').focus();
    }
    
    // ESC to clear search
    if (e.keyCode === 27) {
      $('#search_global').val('').trigger('change');
    }
  });
  
  // Intersection Observer for lazy loading
  if ('IntersectionObserver' in window) {
    var lazyPlots = document.querySelectorAll('.lazy-plot');
    
    var plotObserver = new IntersectionObserver(function(entries, observer) {
      entries.forEach(function(entry) {
        if (entry.isIntersecting) {
          var plotId = entry.target.getAttribute('data-plot-id');
          Shiny.setInputValue('render_plot', plotId);
          observer.unobserve(entry.target);
        }
      });
    });
    
    lazyPlots.forEach(function(plot) {
      plotObserver.observe(plot);
    });
  }
  
  // Performance monitoring
  window.measurePerformance = function() {
    if (window.performance && window.performance.timing) {
      var timing = window.performance.timing;
      var loadTime = timing.loadEventEnd - timing.navigationStart;
      console.log('Page load time:', loadTime + 'ms');
      
      // Send to Shiny for monitoring
      Shiny.setInputValue('performance_metrics', {
        loadTime: loadTime,
        domReady: timing.domContentLoadedEventEnd - timing.navigationStart,
        renderTime: timing.domComplete - timing.domLoading
      });
    }
  };
  
  // Call performance measurement after load
  window.addEventListener('load', function() {
    setTimeout(measurePerformance, 0);
  });
  
  // Custom tooltip styling
  $('[data-toggle="tooltip"]').tooltip({
    container: 'body',
    trigger: 'hover',
    placement: 'auto',
    animation: true
  });
  
  // Print functionality with custom styling
  window.printDashboard = function(elementId) {
    var content = document.getElementById(elementId).innerHTML;
    var printWindow = window.open('', '_blank');
    printWindow.document.write(`
      <html>
        <head>
          <title>RTV Dashboard Report</title>
          <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
          <style>
            body { font-family: 'Inter', sans-serif; }
            @media print {
              .no-print { display: none !important; }
            }
          </style>
        </head>
        <body onload="window.print(); window.close();">
          <div class="container">
            <h1>Raising The Village - Data Report</h1>
            <p>Generated on: ${new Date().toLocaleString()}</p>
            <hr>
            ${content}
          </div>
        </body>
      </html>
    `);
    printWindow.document.close();
  };
  
});

// CSS for loading overlay
var style = document.createElement('style');
style.textContent = `
  .loading-overlay {
    position: fixed;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    background: rgba(255, 255, 255, 0.9);
    z-index: 9999;
    display: flex;
    align-items: center;
    justify-content: center;
    flex-direction: column;
  }
  
  .loading-spinner {
    width: 50px;
    height: 50px;
    border: 4px solid #f3f3f3;
    border-top: 4px solid #4CAF50;
    border-radius: 50%;
    animation: spin 1s linear infinite;
  }
  
  @keyframes spin {
    0% { transform: rotate(0deg); }
    100% { transform: rotate(360deg); }
  }
  
  .loading-text {
    margin-top: 20px;
    font-size: 18px;
    color: #333;
    font-weight: 500;
  }
  
  .highlight {
    background-color: #ffeb3b;
    padding: 2px;
    border-radius: 3px;
  }
`;
document.head.appendChild(style);

// jQuery highlight plugin
jQuery.fn.highlight = function(pat) {
  function innerHighlight(node, pat) {
    var skip = 0;
    if (node.nodeType == 3) {
      var pos = node.data.toUpperCase().indexOf(pat);
      if (pos >= 0) {
        var spannode = document.createElement('span');
        spannode.className = 'highlight';
        var middlebit = node.splitText(pos);
        var endbit = middlebit.splitText(pat.length);
        var middleclone = middlebit.cloneNode(true);
        spannode.appendChild(middleclone);
        middlebit.parentNode.replaceChild(spannode, middlebit);
        skip = 1;
      }
    }
    else if (node.nodeType == 1 && node.childNodes && !/(script|style)/i.test(node.tagName)) {
      for (var i = 0; i < node.childNodes.length; ++i) {
        i += innerHighlight(node.childNodes[i], pat);
      }
    }
    return skip;
  }
  return this.each(function() {
    innerHighlight(this, pat.toUpperCase());
  });
};

jQuery.fn.unhighlight = function() {
  return this.find("span.highlight").each(function() {
    this.parentNode.firstChild.nodeName;
    with (this.parentNode) {
      replaceChild(this.firstChild, this);
      normalize();
    }
  }).end();
};