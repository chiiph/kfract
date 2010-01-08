#  MiniLight Python : minimal global illumination renderer
#
#  Copyright (c) 2007-2008, Harrison Ainsworth / HXA7241 and Juraj Sukop.
#  http://www.hxa7241.org/


from surfacepoint import SurfacePoint
from vector3f import ZERO

class RayTracer(object):

    def __init__(self, scene):
        self.scene_ref = scene

    def get_radiance(self, ray_origin, ray_direction, last_hit=None):
        hit_ref, hit_position = self.scene_ref.get_intersection(ray_origin, ray_direction, last_hit)
        if hit_ref:
            surface_point = SurfacePoint(hit_ref, hit_position)
            local_emission = ZERO if last_hit else surface_point.get_emission(ray_origin, -ray_direction, False)
#            if not local_emission.is_zero():
#                    print "======================"
#                    print "LOCAL_EMISSION != ZERO"
#                ray_origin.echovec()
#                (-ray_direction).echovec()
#                local_emission.echovec()
            illumination = self.sample_emitters(ray_direction, surface_point)
            next_direction, color = surface_point.get_next_direction(-ray_direction)
            reflection = ZERO if next_direction.is_zero() else color * self.get_radiance(surface_point.position, next_direction, surface_point.triangle_ref)
            return reflection + illumination + local_emission
        else:
            return self.scene_ref.get_default_emission(-ray_direction)

    def sample_emitters(self, ray_direction, surface_point):
        emitter_position, emitter_ref = self.scene_ref.get_emitter()
        if emitter_ref:
            emit_direction = (emitter_position - surface_point.position).unitize()
            hit_ref, p = self.scene_ref.get_intersection(surface_point.position, emit_direction, surface_point.triangle_ref)
            emission_in = SurfacePoint(emitter_ref, emitter_position).get_emission(surface_point.position, -emit_direction, True) if not hit_ref or emitter_ref == hit_ref else ZERO

	    print "***************"
	    surface_point.triangle_ref.vertexs[0].echovec()
	    surface_point.triangle_ref.vertexs[1].echovec()
	    surface_point.triangle_ref.vertexs[0].echovec()
	    surface_point.triangle_ref.reflectivity.echovec()
	    surface_point.triangle_ref.emitivity.echovec()

	    surface_point.position.echovec()
	    print "ACA ESTAMOS"
	    emit_direction.echovec()
	    (emission_in * self.scene_ref.emitters_count()).echovec()
	    (-ray_direction).echovec()
	    s = surface_point.get_reflection(emit_direction, emission_in * self.scene_ref.emitters_count(), -ray_direction)
	    s.echovec()
            return s
        else:
            return ZERO
