from surfacepoint import SurfacePoint
from vector3f import Vector3f
from triangle import Triangle

# EMISSION TEST
#tri = Triangle()

#pos = Vector3f(0.226992, 0.545000, 0.306121)

#surfpos = SurfacePoint(tri, pos)

#inDir = Vector3f(0.278000, 0.275000, -0.789000)
#outDir = Vector3f(0.045177, -0.239136, -0.969935)

#surfpos.get_emission(inDir, outDir, False).echovec()

# REFLECTION TEST
tri = Triangle()

pos = Vector3f(0.349778707208, 0.0, 0.000250677650593)

surfpos = SurfacePoint(tri, pos)

inDir = Vector3f(0.15339004845, 0.0204784248673, -0.987953504549)
#inRad = Vector3f(155.231030977, 41.0896754857, 0.512290680565)
#outDir = Vector3f(-0.08556649943, 0.327824061739, -0.940855865008)

#inDir.echovec()
#tri.normal.echovec()
#tri.tangent.echovec()
#print inDir.dot(tri.normal)
#surfpos.get_reflection(inDir, inRad, outDir).echovec()

# NEXTDIRECTION TEST

next, color = surfpos.get_next_direction(-inDir)
next.echovec()
color.echovec()
