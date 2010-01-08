/*
// Copyright (c) www.scratchapixel.com August, 2007
// All rights reserved.
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// * Redistributions of source code must retain the above copyright notice,
//   this list of conditions and the following disclaimer.
// * Redistributions in binary form must reproduce the above copyright notice,
//   this list of conditions and the following disclaimer in the documentation
//   and/or other materials provided with the distribution.
//
//  c++ -o render main.cpp -O3 -Wall
*/

#include <cmath>
#include <cstdlib>
#include <cstdio>
#include <cassert>

typedef struct Normal
{
  Normal() : x(0), y(0), z(0) {}
  Normal(float xx) : x(xx), y(xx), z(xx) {}
  Normal(float xx, float yy, float zz) : x(xx), y(yy), z(zz) {}
  Normal operator * (const float &r) const
  { return Normal(x*r, y*r, z*r); }
  Normal operator - () const
  { return Normal(-x, -y, -z); }
  float x, y, z;
};

typedef struct Vector
{
  Vector() : x(0), y(0), z(0) {}
  Vector(float xx) : x(xx), y(xx), z(xx) {}
  Vector(float xx, float yy, float zz) : x(xx), y(yy), z(zz) {}
  Vector(const Normal &n) : x(n.x), y(n.y), z(n.z) {}
  float length() const { return sqrtf(x*x+y*y+z*z); }
  float dot(const Vector &v) const { return x*v.x+y*v.y+z*v.z; }
  Vector operator * (const float &r) const
  { return Vector(x*r, y*r, z*r); }
  float x, y, z;
  Vector operator - (const Vector &v) const
  { return Vector(x - v.x, y - v.y, z - v.z); }
  Vector operator + (const Vector &v) const
  { return Vector(x + v.x, y + v.y, z + v.z); }
  Vector operator - () const
  { return Vector(-x, -y, -z); }

};

typedef struct Point
{
  Point() : x(0), y(0), z(0) {}
  Point(float xx) : x(xx), y(xx), z(xx) {}
  Point(float xx, float yy, float zz) : x(xx), y(yy), z(zz) {}
  Vector operator - (const Point &p) const
  { return Vector(x - p.x, y - p.y, z - p.z); }
  Point operator - (const Vector &v) const
  { return Point(x - v.x, y - v.y, z - v.z); }
  Point operator + (const Vector &v) const
  { return Point(x+v.x, y+v.y, z+v.z); }
  float x, y, z;
};

void Normalize(Vector *v)
{
  float len2, lenInv;
  len2 = v->x*v->x + v->y*v->y + v->z*v->z;
  if (len2) {
    lenInv = 1.f/sqrtf(len2);
    v->x *= lenInv;
    v->y *= lenInv;
    v->z *= lenInv;
  }
}

typedef struct Color
{
  Color() : r(0), g(0), b(0) {}
  Color(float rr) : r(rr), g(rr), b(rr) {}
  Color(float rr, float gg, float bb) : r(rr), g(gg), b(bb) {}
  Color operator * (const float &f) const { return Color(r * f, g * f, b * f); }
  Color operator + (const Color &c) const 
  { return Color(r + c.r, g + c.g, b + c.b); }
  Color operator * (const Color &c) const 
  { return Color(r * c.r, g * c.g, b * c.b); }
  Color& operator *= (const Color &c) 
  { r *= c.r, g *= c.g, b *= c.b; return *this;}
   Color& operator *= (const float &f) 
  { r *= f, g *= f, b *= f; return *this;}
  float r, g, b;
};

typedef struct Ray
{
  Vector direction;
  Point origin;
};

typedef enum MATERIAL_TYPE { MATTE, DIFFUSE, GLASS };

typedef struct Object
{
  Object(const Point &c, const float &r,
    const Color &col = Color(1), MATERIAL_TYPE matType = MATTE, 
    float l = 0,  float ior = 1.3) :
    center(c), radius(r), radius2(r*r), color(col), materialType(matType), 
    isLight(l), indexOfRefraction(ior) {}
  Point center;
  float radius, radius2;
  Color color;
  MATERIAL_TYPE materialType;
  int isLight;
  float indexOfRefraction;
};

typedef struct Light
{
  const Object *object;
};

static void computePrimRay(const int &i, const int &j, const int &imageWidth, 
  const int &imageHeight, const float &frameAspectRatio, Ray *ray)
{
  float fov = 45;
  float angle = tan(fov * 0.5f * M_PI / 180.0f);
  ray->origin = Point(0);
  float dx = 2 * frameAspectRatio/(float)imageWidth;
  float dy = 2 / (float)imageHeight;
  ray->direction.x = angle * ((i + 0.5) * dx - frameAspectRatio);
  ray->direction.y = angle * (-(j + 0.5) * dy + 1);
  ray->direction.z = 1;
  Normalize(&ray->direction); 
}

int Intersect(const Object *object, const Ray *ray, float *t)
{
  // use geometric method
  Vector oc = object->center - ray->origin;
  // square distance to center of sphere
  float oc2 = oc.dot(oc);
  // distance to point on ray closest to sphere center
  float tca = oc.dot(ray->direction);

  bool outside = oc2 > object->radius2;
  if (tca < 0 && outside) return 0;

  // square distance from sphere center to closest point on ray
  float d2 = oc2 - tca*tca;
  // square distance from perpendicular bisector to center
  float thc = object->radius2 - d2;
  
  if (thc < 0)
    return 0;
  if (outside)
    *t = tca - sqrtf(thc);
  else
    *t = tca + sqrtf(thc);

  if (*t < 0) return 0;
  return 1;
}

#ifdef INFINITY
#undef INFINITY
#endif
#define INFINITY 1e6

#define MAX_DEPTH_LEVEL 3

void snell(
  const Vector &incidentRay,
  const Normal &surfaceNormal, 
  const double &n1,
  const double &n2,
  Vector *reflectionDir, 
  Vector *refractionDir )
{
  float n1n2 = n1 / n2;
  float cost1 = incidentRay.dot(surfaceNormal);
  float cost2 = sqrt(1.0 - (n1n2 * n1n2) * (1.0 - cost1 * cost1));
  *reflectionDir = incidentRay - surfaceNormal * (2 * cost1);
  *refractionDir = incidentRay * n1n2 + surfaceNormal * (cost2 - n1n2 * cost1);
}

void fresnel( const float &etai, const float &etat, const float &cosi,
  const float &cost, float *Kr )
{
  float Rp = ((etat * cosi) - (etai * cost)) / ((etat * cosi) + (etai * cost));
  float Rs = ((etai * cosi) - (etat * cost)) / ((etai * cosi) + (etat * cost));
  *Kr = ( Rp*Rp + Rs*Rs ) * .5;
}

static Color Trace(
  const Ray *ray, 
  const int &depth, 
  const Object **objects, 
  const int &numObjects,
  const Light *light,
  const Color &bgColor)
{
  if (depth > MAX_DEPTH_LEVEL) {
    return bgColor;
  }

  float bias = 0.001;
  const Object *object = NULL;
  float minDistance = INFINITY;
  Point pHit;
  Normal nHit;
  float t;
  for (int k = 0; k < numObjects; k++) {
    if (Intersect(objects[k], ray, &t)) {
      if (t < minDistance) {
        minDistance = t;
        object = objects[k];
      }
    }
  }
  if (object == NULL)
    return bgColor;
  
  pHit = ray->origin + ray->direction * minDistance;
  nHit.x = pHit.x - object->center.x;
  nHit.y = pHit.y - object->center.y;
  nHit.z = pHit.z - object->center.z;
  Normalize((Vector*)&nHit);
  
  if (object->materialType == GLASS) {
    // compute reflection and refraction direction
    Ray reflectionRay, refractionRay;
    reflectionRay.origin = pHit + nHit * bias;
    refractionRay.origin = pHit - nHit * bias;
    snell(ray->direction, -nHit, 1.0, object->indexOfRefraction, 
      &reflectionRay.direction, &refractionRay.direction);
    Normalize(&reflectionRay.direction);
    Normalize(&refractionRay.direction);
    float cosi = ray->direction.dot(-nHit);
    float cost = refractionRay.direction.dot(-nHit);
    float Kr;
    fresnel(1.0, object->indexOfRefraction, cosi, cost, &Kr);
    if (Kr < 0)
      Kr = 0;
    if (Kr > 1)
      Kr = 1;
    Color reflectionColor = Trace(&reflectionRay, depth + 1, objects, 
      numObjects, light, bgColor);
    Color refractionColor = Trace(&refractionRay, depth + 1, objects, 
      numObjects, light, bgColor);
    return object->color * refractionColor * (1-Kr) + reflectionColor * Kr;
  }

  if (object->materialType == MATTE)
    return object->color;

  Ray shadowRay;
  int isInShadow = 0;
  shadowRay.origin.x = pHit.x + nHit.x * bias;
  shadowRay.origin.y = pHit.y + nHit.y * bias;
  shadowRay.origin.z = pHit.z + nHit.z * bias;
  shadowRay.direction = light->object->center - pHit;
  float len = shadowRay.direction.length();
  Normalize(&shadowRay.direction);
  float LdotN = shadowRay.direction.dot(nHit);
  if (LdotN < 0)
    return 0;
  Color lightColor = light->object->color;
  for (int k = 0; k < numObjects; k++) {
    if (Intersect(objects[k], &shadowRay, &t) && !objects[k]->isLight) {
      if (objects[k]->materialType == GLASS)
        lightColor *= objects[k]->color; // attenuate light color by glass color
      else
        isInShadow = 1;
      break;
    }
  }
  lightColor *= 1.f/(len*len);
  return (isInShadow) ? 0 : object->color * lightColor * LdotN;
}

#define MAX_OBJECTS 4

static const Object* createNewObject(const Point &p, const float &r, 
  const Color &col, MATERIAL_TYPE matType, int isLight, Object **&objects, 
  int &numObjects)
{
  assert(numObjects <= MAX_OBJECTS);
  Object *object = new Object(p, r, col, matType, isLight);
  objects[numObjects] = object;
  numObjects++;
  return object;
}

int main (int argc, char * const argv[])
{
  static const int imageWidth = 640;
  static const int imageHeight = 480;
  
  // scene data (objects/light)
  Object **objects = new Object*[MAX_OBJECTS];
  int numObjects = 0;
  createNewObject(
    Point(0, 0, 15), 3, Color(.5, .7, .5), GLASS, 0, objects, numObjects);
  createNewObject(
    Point(-4.5, 4.5, 17), 1.8, Color(1, .3, .3), DIFFUSE, 0, objects, numObjects);
  createNewObject(
    Point(2, -2, 19), 2, Color(.3, 1, .3), DIFFUSE, 0, objects, numObjects);
  Light light;
  light.object = createNewObject(
    Point(3, 3, 13), .5, Color(50), MATTE, 1, objects, numObjects);

  // main render loop
  assert(imageWidth >= imageHeight);
  float frameAspectRatio = imageWidth/(float)imageHeight;
  Color **pixels;
  pixels = new Color*[imageWidth];
  Color bgColor(0.5f, 0.62f, 0.78f);
  for (int i = 0; i < imageWidth; ++i)
    pixels[i] = new Color[imageHeight];
  for (int j = 0; j < imageHeight; ++j) {
    for (int i = 0; i < imageWidth; ++i) {
      // compute primary ray direction
      Ray primRay;
      computePrimRay(i, j, imageWidth, imageHeight, frameAspectRatio, &primRay);
      pixels[i][j] = Trace(&primRay, 0, (const Object**)objects, numObjects, 
        &light, bgColor);
      // clamp
      if (pixels[i][j].r > 1.f) pixels[i][j].r = 1.f;
      if (pixels[i][j].g > 1.f) pixels[i][j].g = 1.f;
      if (pixels[i][j].b > 1.f) pixels[i][j].b = 1.f;
    }
  }

  // save to disk
  FILE *fp;
  fp = fopen("./raytrace.ppm", "w");
  if (fp != NULL) {
    fprintf(fp, "%s\n", "P6");
    fprintf(fp, "%d %d\n", imageWidth, imageHeight);
    fprintf(fp, "%d\n", 255);
    char r, g, b;
    float gamma = 1; // mac
    for (int j = 0; j < imageHeight; ++j) {
      for (int i = 0; i < imageWidth; ++i) {
        pixels[i][j].r = 255 * powf(pixels[i][j].r, gamma);
        pixels[i][j].g = 255 * powf(pixels[i][j].g, gamma);
        pixels[i][j].b = 255 * powf(pixels[i][j].b, gamma);
        r = (pixels[i][j].r > 255) ? 255 : pixels[i][j].r;
        g = (pixels[i][j].g > 255) ? 255 : pixels[i][j].g;
        b = (pixels[i][j].b > 255) ? 255 : pixels[i][j].b;
        fprintf(fp, "%c%c%c", r, g, b);
      }
    }
    fclose(fp);
  }

  // free memory
  for (int i = 0; i < imageWidth; ++i)
    delete [] pixels[i];
  delete [] pixels;
  for (int i = 0; i < numObjects; ++i)
    delete objects[i];
  delete [] objects;
  return 0;
}
