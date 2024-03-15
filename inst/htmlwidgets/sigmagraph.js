

HTMLWidgets.widget({

  name: "sigmagraph",
  type: "output",

  factory: function(el, width, height){

    var graph = new graphology.Graph();
    var s = new Sigma(graph, document.getElementById(el.id));

    return {
      renderValue: function(x){
        if (HTMLWidgets.shinyMode) { // If in Shiny app
          // Remove previous occurences of plots in the <div>
          sigmaID = document.getElementById(el.id)
          while (sigmaID.firstChild) {
            //The list is LIVE so it will re-index each call
            sigmaID.removeChild(sigmaID.firstChild);
          }
          graph = new graphology.Graph();
          //graph.import(x.data);
          s = new Sigma(graph, sigmaID, {
              labelColor: { color: "#888888" }, 
              zIndex: true
            });
          s.refresh();
        }

        s.setSetting('minEdgeSize', x.options.minEdgeSize);
        s.setSetting('maxEdgeSize', x.options.maxEdgeSize);
        s.setSetting('minNodeSize', x.options.minNodeSize);
        s.setSetting('maxNodeSize', x.options.maxNodeSize);
        s.setSetting('doubleClickEnabled', x.options.doubleClickZoom);
        s.setSetting('mouseWheelEnabled', x.options.mouseWheelZoom);
        s.graph.import(x.data);

        if (x.options.neighborEvent != 'None'){
          s.graph.nodes().forEach(function(n) {
            s.graph.setNodeAttribute(n, 'originalColor', s.graph.getNodeAttribute(n, 'color'));
          });
          s.graph.edges().forEach(function(e) {
            s.graph.setEdgeAttribute(e, 'originalColor', s.graph.getEdgeAttribute(e, 'color'));
            //e.type = x.options.edgeArrows;
          });
          s.on(x.options.neighborStart, function(e) {
            var nodeId = e.node,
                toKeep = s.graph.neighbors(nodeId);
            toKeep.push(e.node);
            s.graph.nodes().forEach(function(n) {
              if (toKeep.includes(n))
                s.graph.setNodeAttribute(n, 'color', s.graph.getNodeAttribute(n, 'originalColor'));
              else
                s.graph.setNodeAttribute(n, 'color', '#eee');
            });
            s.graph.edges().forEach(function(e) {
              if (toKeep.includes(s.graph.source(e)) && toKeep.includes(s.graph.target(e)))
                s.graph.setEdgeAttribute(e, 'color', s.graph.getEdgeAttribute(e, 'originalColor'));
              else
                s.graph.setEdgeAttribute(e, 'color', '#eee');
            });
            s.refresh();
          });
          s.on(x.options.neighborEnd, function(e) {
            s.graph.nodes().forEach(function(n) {
                s.graph.setNodeAttribute(n, 'color', s.graph.getNodeAttribute(n, 'originalColor'));
            });
            s.graph.edges().forEach(function(e) {
                s.graph.setEdgeAttribute(e, 'color', s.graph.getEdgeAttribute(e, 'originalColor'));
            });
            s.refresh();
          });

          if(HTMLWidgets.shinyMode){
            if(x.options.sigmaEvents){
              if(x.options.sigmaEvents == 'clickNode'){
                s.on("clickNode", function(d){
                  Shiny.onInputChange('node_data', d.data.node)
                })
              }
              if(x.options.sigmaEvents == 'hoverNode'){
                s.on("overNode", function(d){
                  Shiny.onInputChange('node_data', d.data.node)
                })
              }
            }
          }
        }


        s.refresh();
      },
      resize: function(width, height){
        for(var name in s.renderers)
          s.renderers[name].resize(width, height);
      },
      s: s
    };
  }
})
