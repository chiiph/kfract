from triangle import Triangle
from camera import Camera
from random import random
from math import pi, tan
from vector3f import Vector3f

#cam = Camera()

tri = Triangle()

vec1 = Vector3f(0.556, 0.401054805321, 0.40692763018)
vec2 = Vector3f(-0.745565615449, 0.432850996024, -0.506726680076)
print tri.get_intersection(vec1, vec2)
tri.get_sample_point().echovec()
print tri.area
tri.tangent.echovec()
tri.normal.echovec()
