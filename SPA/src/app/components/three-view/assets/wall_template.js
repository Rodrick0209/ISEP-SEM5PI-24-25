import * as THREE from "three";

/*
 * parameters = {
 *  textureUrl: String
 * }
 */

export default class Wall {
    constructor(parameters) {
        for (const [key, value] of Object.entries(parameters)) {
            this[key] = value;
        }

        const texture = new THREE.TextureLoader().load(this.textureUrl);
        texture.colorSpace = THREE.SRGBColorSpace;
        texture.magFilter = THREE.LinearFilter;
        texture.minFilter = THREE.TrilinearFilter;

        // Create a group for the wall object
        this.object = new THREE.Group();

        // Top Part (White)
        let geometry = new THREE.PlaneGeometry(1, 1); // 1/3 of the total height
        let material = new THREE.MeshPhongMaterial({ color: 0xffffff, map: texture });
        let topFace = new THREE.Mesh(geometry, material);
        topFace.position.set(0.0, 0.33, 0.025); // Position at the top part
        this.object.add(topFace);

        // Middle Part (Blue line)
        geometry = new THREE.PlaneGeometry(1, 0.2); // 1/3 of the total height
        material = new THREE.MeshPhongMaterial({ color: 0x68B5E3 }); // Blue color
        let middleFace = new THREE.Mesh(geometry, material);
        middleFace.position.set(0.0, 0.0, 0.04); // Position in the middle
        this.object.add(middleFace);

        // Bottom Part (White)
        geometry = new THREE.PlaneGeometry(1, 1); // 1/3 of the total height
        material = new THREE.MeshPhongMaterial({ color: 0xffffff, map: texture });
        let bottomFace = new THREE.Mesh(geometry, material);
        bottomFace.position.set(0.0, -0.33, 0.025); // Position at the bottom part
        this.object.add(bottomFace);

        let face = new THREE.Mesh().copy(topFace, false);
        face.rotation.y = Math.PI;
        face.position.set(0.0, 0.33, -0.025);
        this.object.add(face);

        face = new THREE.Mesh().copy(middleFace, false);
        face.rotation.y = Math.PI;
        face.position.set(0.0, 0.0, -0.04);
        this.object.add(face);

        face = new THREE.Mesh().copy(bottomFace, false);
        face.rotation.y = Math.PI;
        face.position.set(0.0, -0.33, -0.025);
        this.object.add(face);

        // Adjust sides (left and right)
        let points = new Float32Array([
            -0.5, -0.75, 0.025,
            -0.5, 0.75, 0.025,
            -0.52, 0.75, 0.0,
            -0.52, -0.75, 0.0,

            -0.52, 0.75, 0.0,
            -0.5, 0.75, -0.025,
            -0.5, -0.75, -0.025,
            -0.52, -0.75, 0.0
        ]);

        let normals = new Float32Array([
            -0.707, 0.0, 0.707,
            -0.707, 0.0, 0.707,
            -0.707, 0.0, 0.707,
            -0.707, 0.0, 0.707,
            -0.707, 0.0, -0.707,
            -0.707, 0.0, -0.707,
            -0.707, 0.0, -0.707,
            -0.707, 0.0, -0.707
        ]);

        let indices = [
            0, 1, 2,
            2, 3, 0,
            4, 5, 6,
            6, 7, 4
        ];

        geometry = new THREE.BufferGeometry().setAttribute("position", new THREE.BufferAttribute(points, 3));
        geometry.setAttribute("normal", new THREE.BufferAttribute(normals, 3));
        geometry.setIndex(indices);
        material = new THREE.MeshPhongMaterial({ color: 0xffffff, map: texture });
        
        let sideFace = new THREE.Mesh(geometry, material);
        this.object.add(sideFace);

        // Right Side (Duplicate of the side face but rotated 180 degrees)
        let rightFace = new THREE.Mesh().copy(sideFace, false);
        rightFace.rotation.y = Math.PI;
        this.object.add(rightFace);

        // Top edge adjustment if necessary
        points = new Float32Array([
            -0.5, 0.75, 0.0,
            -0.48, 0.75, 0.025,
            0.48, 0.75, 0.025,
            0.5, 0.75, 0.0,
            0.48, 0.75, -0.025,
            -0.48, 0.75, -0.025
        ]);

        normals = new Float32Array([
            0.0, 1.0, 0.0,
            0.0, 1.0, 0.0,
            0.0, 1.0, 0.0,
            0.0, 1.0, 0.0,
            0.0, 1.0, 0.0,
            0.0, 1.0, 0.0
        ]);

        indices = [
            0, 1, 5,
            5, 1, 3,
            4, 5, 3,
            3, 4, 2
        ];

        geometry = new THREE.BufferGeometry().setAttribute("position", new THREE.BufferAttribute(points, 3));
        geometry.setAttribute("normal", new THREE.BufferAttribute(normals, 3));
        geometry.setIndex(indices);
        let topEdgeFace = new THREE.Mesh(geometry, material);
        this.object.add(topEdgeFace);
    }
}