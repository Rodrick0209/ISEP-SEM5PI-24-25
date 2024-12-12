import * as THREE from "three";

export default class Lights {
  constructor(parameters) {
    // Assign all parameters to the class
    for (const [key, value] of Object.entries(parameters)) {
      Object.defineProperty(this, key, {
        value: value,
        writable: true,
        configurable: true,
        enumerable: true,
      });
    }

    // Create a group of lights
    this.object = new THREE.Group();

    // Create the ambient light
    this.object.ambientLight = new THREE.AmbientLight(
      this.ambientLight.color,
      this.ambientLight.intensity
    );
    this.object.add(this.object.ambientLight);

    // Create multiple point lights in a loop (from pointLight1 to pointLightN)
    this.pointLights = []; // Array to store point lights for reference
    for (let i = 1; i <= 10; i++) {
      const lightParam = this[`pointLight${i}`]; // Access parameters dynamically
      if (lightParam) {
        const pointLight = new THREE.PointLight(
          lightParam.color,
          lightParam.intensity,
          lightParam.distance
        );

        pointLight.position.set(
          lightParam.position.x,
          lightParam.position.y,
          lightParam.position.z
        );

        // Enable shadows for the light
        pointLight.castShadow = true;
        pointLight.shadow.mapSize.width = 512;
        pointLight.shadow.mapSize.height = 512;
        pointLight.shadow.camera.near = 5.0;
        pointLight.shadow.camera.far = 15.0;

        // Add the point light to the scene and store it
        this.object.add(pointLight);
        this.pointLights.push(pointLight);
      }
    }

    // Create the spot light
    this.object.spotLight = new THREE.SpotLight(
      this.spotLight.color,
      this.spotLight.intensity,
      this.spotLight.distance,
      this.spotLight.angle,
      this.spotLight.penumbra
    );
    this.object.spotLight.position.set(
      this.spotLight.position.x,
      this.spotLight.position.y,
      this.spotLight.position.z
    );
    this.object.spotLight.target.position.set(
      this.spotLight.direction.x,
      this.spotLight.direction.y,
      this.spotLight.direction.z
    );

    // Enable shadows for the spot light
    this.object.spotLight.castShadow = true;
    this.object.spotLight.shadow.mapSize.width = 512;
    this.object.spotLight.shadow.mapSize.height = 512;
    this.object.spotLight.shadow.camera.near = 5.0;
    this.object.spotLight.shadow.camera.far = 15.0;

    this.object.add(this.object.spotLight);
    this.object.add(this.object.spotLight.target);
  }
}