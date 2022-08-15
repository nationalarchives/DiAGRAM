(function() {
	'use strict';

	var edgesToVertexes = {
		'11-21': ['Digital Object', 'File Format'],
		'18-56': ['Operating Environment', 'Storage Life'],
		'19-61': ['Replication and Refreshment', 'Storage Life'],
		'20-66': ['Physical Disaster', 'Storage Life'],
		'22-75': ['Storage Medium', 'Storage Life'],
		'26-85': ['Technical Skills', 'Obsolescence'],
		'27-90': ['Storage Medium', 'Obsolescence'],
		'28-95': ['Technical Skills', 'Tools to Render'],
		'29-100': ['File Format', 'Tools to Render'],
		'34-121': ['Digital Object', 'Conditions of Use'],
		'35-126': ['Digital Object', 'Content Metadata'],
		'36-131': ['Information Management', 'Identity'],
		'37-136': ['Content Metadata', 'Identity'],
		'38-141': ['Information Management', 'Integrity'],
		'39-146': ['System Security', 'Integrity'],
		'40-151': ['Checksum', 'Integrity'],
		'48-172': ['Technical Skills', 'Technical Metadata'],
		'49-177': ['Technical Metadata', 'Renderability'],
		'50-182': ['Bit Preservation', 'Renderability'],
		'51-187': ['Storage Life', 'Bit Preservation'],
		'52-192': ['Obsolescence', 'Bit Preservation'],
		'53-197': ['Tools to Render', 'Renderability'],
		'54-202': ['Conditions of Use', 'Intellectual Control'],
		'56-207': ['Integrity', 'Bit Preservation'],
		'57-212': ['Identity', 'Intellectual Control']
	};

	var svg = document.querySelector('#diagram-network');
	var xmlns = svg.getAttribute('xmlns');

	var mainDescription = document.querySelector('#diagram-network-description');
	mainDescription.textContent += ' The dropdown menu below this image lists the available nodes. Please use it to select a node.';

	var edgesGroup = document.createElementNS(xmlns, 'g');
	edgesGroup.setAttribute('id', 'edges');
	edgesGroup.setAttribute('aria-hidden', true);
	var vertexesGroup = document.createElementNS(xmlns, 'g');
	vertexesGroup.setAttribute('id', 'vertexes');
	vertexesGroup.setAttribute('aria-hidden', true);
	svg.appendChild(edgesGroup);
	svg.appendChild(vertexesGroup);


	svg.querySelectorAll('ellipse').forEach(function(ellipse) {
		var vertex = ellipse.parentNode;
		vertexesGroup.appendChild(vertex);
		vertex.classList.add('vertex');
		var name = vertex.querySelector('desc').innerHTML;
		vertex.name = name;
		vertex.edges = { in: [], out: [] };
		Object.keys(edgesToVertexes).forEach(function(key) {
			var edgeToVertex = edgesToVertexes[key];
			var index = edgeToVertex.indexOf(name);
			if (index !== -1) {
				vertex.edges[index === 0 ? 'out' : 'in' ].push(svg.querySelector('#shape' + key));
				edgeToVertex[index] = vertex;
			} 
		});
	});

	svg.querySelectorAll('.st8').forEach(function(path) {
		var edge = path.parentNode;
		edgesGroup.appendChild(edge);
		edge.classList.add('edge');
		var id = edge.getAttribute('id');
		var vertexes = edgesToVertexes[id.split('shape')[1]];
		edge.start = vertexes[0];
		edge.end = vertexes[1];
		edge.name = vertexes[0].name + ' >> ' + vertexes[1].name;
	});


	window.getSubnetwork = function(vertex) {
		var subnetwork = {};
		subnetwork[vertex.name] = vertex;
		
		var edge, name, nextVertex;

		var edges = vertex.edges.in.slice();
		while (edges.length) {
			edge = edges.shift();
			name = edge.name;
			if (subnetwork[name]) { continue; }
			subnetwork[name] = edge;

			nextVertex = edge.start;
			if (!subnetwork[nextVertex.name]) {
				subnetwork[nextVertex.name] = nextVertex;
				edges.push.apply(edges, nextVertex.edges.in);	
			}
		}

		edges = vertex.edges.out.slice();
		while (edges.length) {
			edge = edges.shift();
			name = edge.name;
			if (subnetwork[name]) { continue; }
			subnetwork[name] = edge;

			nextVertex = edge.end;
			if (!subnetwork[nextVertex.name]) {
				subnetwork[nextVertex.name] = nextVertex;
				edges.push.apply(edges, nextVertex.edges.out);	
			}
		}		

		return Object.keys(subnetwork).map(function(k) { return subnetwork[k]; });
	};
})();