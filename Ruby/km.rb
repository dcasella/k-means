#!/usr/bin/ruby
#encoding utf-8

def km (observations, k)
	return nil if observations.nil?
	return [observations] if k == 1
	return observations if observations.length == k
	raise "Can't compute clusters" if observations.length < k

	clusters_map = []
	cs = init(observations, k)

	while true do
		new_clusters_map = partition(observations, cs)

		if clusters_map == new_clusters_map
			return map_clusters(clusters_map, observations, k)
		end

		clusters_map = new_clusters_map
		cs = re_centroids(clusters_map, observations, k)
	end
end

def centroid (observations)
	if observations
		observations.transpose.map{ |x| x.reduce(:+) }
							  .map{ |x| x / observations.length }
	end
end

def vsum (vector1, vector2)
	[vector1, vector2].transpose.map{ |x| x.reduce(:+) }
end

def vsub (vector1, vector2)
	[vector1, vector2].transpose.map{ |x| x.reduce(:-) }
end

def innerprod (vector1, vector2)
	[vector1, vector2].transpose.map{ |x| x.reduce(:*) }.reduce(:+)
end

def norm (vector)
	Math.sqrt(innerprod(vector, vector))
end

def init (observations, k)
	centroids = []
	_observations = observations.clone

	k.times do |i|
		r = _observations.delete _observations.sample
		centroids[i] = r
	end

	return centroids
end

def partition (observations, cs)
	clusters_map = []
	centr = []

	observations.length.times do |i|
		min_distance = Float::MAX

		cs.length.times do |c|
			if (curr_distance = norm(vsub(observations[i], cs[c]))) < min_distance
				min_distance = curr_distance
				centr = c
			end
		end

		clusters_map[i] = centr
	end

	return clusters_map
end

def re_centroids (clusters_map, observations, k)
	centroids = []

	k.times do |c|
		temp = clusters_map.each_index.select{ |i| clusters_map[i] == c }
		centroids[c] = centroid(observations.values_at(*temp))
	end

	return centroids
end

def map_clusters (clusters_map, observations, k)
	clusters = []

	k.times do |i|
		clusters[i] = map_cluster(clusters_map, observations, i)
	end

	return clusters
end

def map_cluster (clusters_map, observations, c)
	temp = clusters_map.each_index.select{ |i| clusters_map[i] == c }
	cluster = observations.values_at(*temp)

	return cluster
end
