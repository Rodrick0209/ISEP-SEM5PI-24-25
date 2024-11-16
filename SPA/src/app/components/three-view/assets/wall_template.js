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

        // Cria um grupo de objetos
        this.object = new THREE.Group();

        // Frente
        let geometry = new THREE.PlaneGeometry(1, 1.5); // Reduzido para 1.5 em Y
        let material = new THREE.MeshPhongMaterial({ color: 0xffffff, map: texture });
        let face = new THREE.Mesh(geometry, material);
        face.position.set(0.0, 0.0, 0.025);
        this.object.add(face);

        // Traseira
        face = new THREE.Mesh().copy(face, false);
        face.rotation.y = Math.PI;
        face.position.set(0.0, 0.0, -0.025);
        this.object.add(face);

        // Lados Esquerdo e Direito ajustados
        let points = new Float32Array([
            -0.5, -0.75, 0.025, // Ajustado para -0.75
            -0.5, 0.75, 0.025,  // Ajustado para 0.75
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
        face = new THREE.Mesh(geometry, material);
        this.object.add(face);

        // Lado Direito
        face = new THREE.Mesh().copy(face, false);
        face.rotation.y = Math.PI;
        this.object.add(face);

        // Ajuste do Topo
        points = new Float32Array([
            -0.5, 0.75, 0.0,   // Ajustado para 0.75
            -0.48, 0.75, 0.025,
            0.48, 0.75, 0.025,
            0.5, 0.75, 0.0,    // Ajustado para 0.75
            0.48, 0.75, -0.025,
            -0.48, 0.75, -0.025 // Ajustado para 0.75
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
        face = new THREE.Mesh(geometry, material);
        this.object.add(face);
    }
}
