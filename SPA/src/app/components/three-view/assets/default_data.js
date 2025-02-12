import * as THREE from "three";
import Orientation from "./orientation.js";


export const generalData = {
    setDevicePixelRatio: false
}

export const mazeData = {
    url: "./assets/mazes/Loquitas.json",
    credits: "Maze designed by Cecília Fernandes and Nikita.",
    scale: new THREE.Vector3(1.0, 1.0, 1.0)
}

export const playerData = {
    url: "./assets/models/gltf/RobotExpressive/RobotExpressive.glb",
    credits: "Model and related code snippets created by <a href='https://www.patreon.com/quaternius' target='_blank' rel='noopener'>Tomás Laulhé</a>. CC0 1.0. Modified by <a href='https://donmccurdy.com/' target='_blank' rel='noopener'>Don McCurdy</a>.",
    eyeHeight: 0.8, // fraction of character height
    scale: new THREE.Vector3(0.1, 0.1, 0.1),
    walkingSpeed: 0.75,
    initialDirection: 0.0, // Expressed in degrees
    turningSpeed: 75.0, // Expressed in degrees / second
    runningFactor: 2.0, // Affects walking speed and turning speed
    keyCodes: { fixedView: "Digit1", firstPersonView: "Digit2", thirdPersonView: "Digit3", topView: "Digit4", viewMode: "KeyV", userInterface: "KeyU",  help: "KeyH", statistics: "KeyS", run: "KeyR", left: "ArrowLeft", right: "ArrowRight", backward: "ArrowDown", forward: "ArrowUp", jump: "KeyJ", yes: "KeyY", no: "KeyN", wave: "KeyW", punch: "KeyP", thumbsUp: "KeyT" }
}

export const lightsData = {
    ambientLight: { color: 0xffffff, intensity: 1.0 },
    pointLight1: { color: 0xffffff, intensity: 1.0, distance: 0.0, position: new THREE.Vector3(0.0, 0.0, 0.0) },
    pointLight2: { color: 0xffffff, intensity: 1.0, distance: 0.0, position: new THREE.Vector3(0.0, 0.0, 0.0) },
    spotLight: { color: 0xffffff, intensity: 1.0, distance: 0.0, angle: Math.PI / 3.0, penumbra: 0.0, position: new THREE.Vector3(0.0, 0.0, 0.0), direction: 0.0 } // angle and direction expressed in radians
}

export const fogData = {
    enabled: false,
    color: 0xe0e0e0,
    near: 0.1,
    far: 14.0
}


export const doorData = {
    url: "./assets/models/gltf/door/door.glb",
    scale: new THREE.Vector3(0.003, 0.00312, 0.005),
    doorDirection: 0.0, // Expressed in degrees
    doorName:""
}

export const bedData = {
    url: "./assets/models/gltf/bed/simple_bed/scene.gltf",
    scale: new THREE.Vector3(0.004, 0.0035, 0.0039),
    bedDirection: 0.0, // Expressed in degrees
    bedName:""
}

export const doctorData = {
    url: "./assets/models/gltf/doctor/doctor.glb",
    scale: new THREE.Vector3(0.2, 0.2, 0.2),
    doctorDirection : 0.0
}


export const bedWithPatientData = {
    url: "./assets/models/gltf/bed/bed_patient/untitled.gltf",
    scale: new THREE.Vector3(0.004, 0.0035, 0.0039),
    bedWithPatientDirection: 0.0, // Expressed in degrees
}

export const tablesSurgeryData = {
    url: "./assets/models/gltf/medicalEquipment/tablesSurgery.glb",
    scale: new THREE.Vector3(0.3, 0.3, 0.3),
    tablesCirurgyDirection: 0.0, // Expressed in degrees
}


export const waitingChairData = {
    url: "./assets/models/gltf/waitingChair/waitingChair.glb",
    scale: new THREE.Vector3(0.01, 0.01, 0.01),
    waitingChairDirection: 0.0, // Expressed in degrees
}

export const vendingMachineData={
    url: "./assets/models/gltf/vending_machine/scene.gltf",
    scale: new THREE.Vector3(0.004, 0.004, 0.004),
    vendingMachineDirection: 0.0, // Expressed in degrees
}

export const boardData={
    url: "./assets/models/gltf/board/safety_notice_board.glb",
    scale: new THREE.Vector3(0.005, 0.005, 0.005),
    vendingMachineDirection: 0.0, // Expressed in degrees
}



export const cameraData = {
    view: "fixed", // Fixed view: "fixed"; first-person view: "first-person"; third-person view: "third-person"; top view: "top"; mini-map: "mini-map"
    multipleViewsViewport: new THREE.Vector4(0.0, 0.0, 1.0, 1.0), // Viewport position and size: fraction of window width and window height; MUST BE REDEFINED when creating an instance of ThumbRaiser() so that each view is assigned a different viewport
    target: new THREE.Vector3(0.0, 0.0, 0.0), // Target position
    initialOrientation: new Orientation(116.0, -32.0), // Horizontal and vertical orientation and associated limits (expressed in degrees)
    orientationMin: new Orientation(-180.0, -90.0),
    orientationMax: new Orientation(180.0, 0.0),
    initialDistance: 20, // Distance to the target and associated limits
    distanceMin: 4.0,
    distanceMax: 20.0,
    initialZoom: 1, // Zoom factor and associated limits
    zoomMin: 0.5,
    zoomMax: 2.0,
    initialFov: 32.0, // Field-of-view (expressed in degrees)
    near: 0.01, // Front clipping plane
    far: 100.0 // Back clipping plane
}