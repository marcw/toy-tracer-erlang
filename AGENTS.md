# AGENTS.md  

## Educational Raytracer Project Specification  

**Target Audience**: Software engineers
**Project type:** Minimal CPU-based Raytracer in Erlang

---

## 1. Pedagogical Context

This project introduces students to:

- Core Erlang programming
- Basic mathematical reasoning in 3D (vectors, rays, normals)
- Rendering logic (ray-primitive intersection, lighting)
- File output (writing an image to disk)

The final result is a **portable and minimal raytracer** that generates a simple `.tga` image on any computer (Windows, macOS, Linux) with **no dependencies** and **no external libraries**.  

---

## 2. Technical Goals

The raytracer must:

- Be written **entirely in standard Erlang**  
- Produce a `.tga` image as output, containing the rendered scene  
- Avoid any dynamic dependency (no SDL, OpenGL, stb, etc.)  
- Require **no makefiles**, **no cmake**, **no external build system**

---

## 3. Core Components

### 3.1. Main Renderer Loop

Responsible for:

- Setting up camera parameters (position, FOV, aspect ratio)
- Creating an image buffer (RGBA 32-bit)
- Looping over each pixel, generating a ray
- Writing the final image as uncompressed TGA

### 3.2. Supported Primitives

The renderer must handle **only these 5 geometric primitives**:

- Plane  
- Cube  
- Sphere  
- Cylinder  
- Torus  

Each primitive has:

- A world-space position and orientation  
- A function returning ray intersection (boolean + hit distance)  
- A normal computation function  

### 3.3. Lighting

- Only **point lights** are supported.  
- Shadows may be implemented by checking occlusion between hit point and light.  

### 3.4. Material System

Each primitive references a `Material` data structure with that kind of data: albedo (3d vector), roughness (float), metalness (float).

Shading uses a simplified **"Principled PBR"** model inspired by Blender, but implemented directly in Erlang (no external math libs).  
Approximate Fresnel/roughness effects are acceptable.  

Textures are **not supported**.

---

## 5. Rendering Model

- Camera: simple pinhole camera at origin, looking along +Z  
- Rays: primary rays only (no recursion, no reflections, no refraction)  
- Shading: direct illumination from one point light  
- Shadows: optional (cast shadow rays)  
- Background: uniform color or gradient  

---

## 6. Image Output

Output must be a **TGA 32-bit (uncompressed)** file.  
Header and footer are written manually (no external library).  
Each pixel is written as BGRA (1 byte per channel).  
File name: `output.tga`.

---

## 7. Math Utilities

Students will need to implement:

- Vector operations: addition, subtraction, normalization, dot, cross
- Basic math helpers: clamp, mix, reflect
- Intersection math for all primitives listed above  

All math should be written in plain Erlang. No external libs.

---

## 8. Scene Definition

All scene setup happens in an erlang file:

- Create objects by filling an array of Primitives.
- Each Primitive has a position, size, material, and type
- Example default scene: a **cube on a checkerboard plane**, one point light, camera at origin

Example in C (but USE ONLY ERLANG yourself):
```c
void setup_scene(Scene* scene) {
    // Ground
    scene->objects[0] = make_plane(vec3(0,-1,0), vec3(0,1,0), material_checkerboard());

    // Cube
    scene->objects[1] = make_cube(vec3(0,0,5), 1.0f, material_metal());

    // Light
    scene->light = make_light(vec3(5,5,0), 1.0f);
}
```

---

## 10. Extensions (Optional)

For motivated software engineers, optional improvements may include:

- Multiple lights  
- Ambient occlusion approximation  
- Reflection/refraction recursion  
- Multi-threading  
- Anti-aliasing by sampling multiple rays per pixel  
- Gamma correction

---

## 11. Platform Notes

No IDE required, but the code should compile and debug easily in **Visual Studio**, **VS Code**, or **Code::Blocks**.

---

## 12. Deliverables

Each student must:

1. Write their own scene in Erlang
2. Compile and run their code to produce `output.tga`
3. Comment their code clearly (in English)
4. Submit:
   - All source code files
   - One `output.tga` file

---

## 13. Code Commenting Rules

- All comments **must be written in English**.  
- Comments should explain *why* the code exists, not only *what* it does.  
- Example:

  ```erlang
  %% Compute the intersection between a ray and a sphere
  %% Returns true if hit, and updates the hit record with distance and normal
  ```

---

## 14. Evaluation Criteria

| Criterion | Weight | Description |
|------------|---------|-------------|
| Code structure | 25% | Proper separation of concerns in source code files|
| Readability | 20% | Clear naming and English comments |
| Correctness | 25% | Raytracer produces a valid image |
| Creativity | 15% | Interesting or aesthetic scene composition |
| Bonus | 15% | Optional features (shadows, reflections, etc.) |

---

## 15. References

- Shirley & Morley, *Fundamentals of Computer Graphics*  
- Smallpt path tracer (Kevin Beason, 2008) – reference for minimalism  
- Blender Principled BSDF documentation  
- Paul Bourke’s geometry intersection formulas

---

## 16. Constraints Summary

✅ **Allowed:**  

- Erlang.
- Erlang Standard libraries.
- Hardcoded data (no external assets)  

❌ **Not allowed:**  

- C / C++ or object-oriented constructs  
- External dependencies or libraries  
- Textures, GUI, or real-time display  
- Build systems (Make, CMake, etc.)

---

**End of AGENTS.md**
