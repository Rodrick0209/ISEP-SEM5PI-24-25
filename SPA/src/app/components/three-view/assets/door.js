import * as THREE from "three";
import { GLTFLoader } from "three/addons/loaders/GLTFLoader.js";

export default class Door {
    constructor(parameters) {
        
        this.onLoad = function (description) {
            this.object = description.scene;

            // Define a cor cinza claro
            const grayColor = new THREE.Color(0xaaaaaa);

            // Aplica a cor cinza claro para todos os materiais do modelo
            this.object.traverse((child) => {
                if (child.isMesh && child.material) {
                    if (Array.isArray(child.material)) {
                        child.material.forEach(mat => mat.color.set(grayColor));
                    } else {
                        child.material.color.set(grayColor);
                    }
                }
            });

            // Caixa delimitadora para ajustar a escala e posição
            const box = new THREE.Box3();
            box.setFromObject(this.object);

            const size = new THREE.Vector3();
            box.getSize(size);

            size.x = 3.0;
            size.y = 4.1;
            size.z = 2.6;

            this.radius = size.x / 2.0 * this.scale.x;
            this.eyeHeight *= size.y * this.scale.y;

            this.object.scale.set(this.scale.x, this.scale.y, this.scale.z);
            this.loaded = true;
        }

        this.onProgress = function (url, xhr) {
            //console.log("Resource '" + url + "' " + (100.0 * xhr.loaded / xhr.total).toFixed(0) + "% loaded.");
        }

        this.onError = function (url, error) {
            console.error("Error loading resource " + url + " (" + error + ").");
        }

        for (const [key, value] of Object.entries(parameters)) {
            this[key] = value;
        }
        this.initialDirection = THREE.MathUtils.degToRad(this.initialDirection);
        this.keyStates = { fixedView: false, firstPersonView: false, thirdPersonView: false, topView: false, viewMode: false, miniMap: false, statistics: false, userInterface: false, help: false, run: false, left: false, right: false, backward: false, forward: false, jump: false, yes: false, no: false, wave: false, punch: false, thumbsUp: false };
        this.loaded = false;

        const loader = new GLTFLoader();

        loader.load(
            this.url,
            description => this.onLoad(description),
            xhr => this.onProgress(this.url, xhr),
            error => this.onError(this.url, error)
        );
    }
}
