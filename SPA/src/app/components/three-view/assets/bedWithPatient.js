import * as THREE from "three";
import { GLTFLoader } from "three/addons/loaders/GLTFLoader.js";

export default class BedWithPatient {
  constructor(parameters) {
    this.object = null; // Will hold the loaded model
    this.loaded = false;
    this.loadPromise = this.loadModel(parameters); // Start the model loading
  }

  loadModel(parameters) {
    return new Promise((resolve, reject) => {
      const loader = new GLTFLoader();

      loader.load(
        parameters.url,
        (description) => {
          this.object = description.scene;
          this.animations = description.animations;

          // Enable shadows
          this.setShadow(this.object);

          // Set object scale
          this.object.scale.set(parameters.scale.x, parameters.scale.y, parameters.scale.z);

          // Compute bounding box for size
          const box = new THREE.Box3();
          box.setFromObject(this.object);

          const size = new THREE.Vector3();
          box.getSize(size);

          size.x = 1.0;
          size.y = 1.1;
          size.z = 1.6;

          this.radius = size.x / 2.0 * parameters.scale.x;
          this.eyeHeight = size.y * parameters.scale.y;

          this.loaded = true;
          resolve(this.object); // Resolve the Promise with the loaded object
        },
        undefined, // Progress callback
        (error) => {
          console.error("Error loading resource:", error);
          reject(error);
        }
      );
    });
  }

  async getObject() {
    return this.loadPromise; // Allow awaiting the loaded object
  }

  setShadow(object) {
    object.traverseVisible((child) => {
      if (child instanceof THREE.Mesh) {
        child.castShadow = true;
        child.receiveShadow = false;
      }
    });
  }
}