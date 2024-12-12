import * as THREE from "three";
import * as GSAP from "gsap";
import Stats from "three/addons/libs/stats.module.js";
import Orientation from "./orientation.js";
import {
  generalData,
  mazeData,
  playerData,
  lightsData,
  fogData,
  cameraData,
  doorData,
  bedData,
  doctorData,
  bedWithPatientData,
  tablesSurgeryData,
} from "./default_data.js";
import { merge } from "./merge.js";
import Maze from "./maze_template.js";
import Player from "./player_template.js";
import Lights from "./lights_template.js";
import Fog from "./fog_template.js";
import Camera from "./camera_template.js";
import Animations from "./animations_template.js";
import Door from "./door.js";

import UserInterface from "./user_interface_template.js";
import Bed from "./bed.js";
import Doctor from "./doctor.js";
import BedWithPatient from "./bedWithPatient.js";
import SurgeryTable from "./surgeryTable.js";
import { tangentGeometry } from "three/webgpu";

let rooms = null;

export default class ThumbRaiser {
  constructor(
    parameters,
    generalParameters,
    mazeParameters,
    playerParameters,
    lightsParameters,
    fogParameters,
    fixedViewCameraParameters,
    //firstPersonViewCameraParameters,
    //thirdPersonViewCameraParameters,
    topViewCameraParameters,
    miniMapCameraParameters,
    doorParameters,
    bedParameters,
    doctorParameters,
    bedWithPatientParameters,
    tablesSurgeryDataParameters,
    date,
    time
  ) {
    this.onLoad = async function (description) {
      rooms = description.rooms;
      const apiUrl = "https://localhost:5001/api/OperationRoom/OccupiedRooms";

      // Construct the URL with date and time as query parameters
      const urlWithParams = `${apiUrl}?date=${date.date}&time=${time.time}`;

      const response = await fetch(urlWithParams);

      const data = await response.json();
      await this.updateRoomOccupancy(data);

      for (const room of rooms) {
        console.log("Description: " + room.name + " " + room.isOccupied);
      }

      this.map = description.map;
      this.size = description.size;

      this.generalParameters = merge({}, generalData, generalParameters);
      this.mazeParameters = merge({}, mazeData, mazeParameters);
      this.bedWithPatientParameters = merge(
        {},
        bedWithPatientData,
        bedWithPatientParameters
      );
      this.playerParameters = merge({}, playerData, playerParameters);
      this.doorParameters = merge({}, doorData, doorParameters);
      this.bedParameters = merge({}, bedData, bedParameters);

      this.lightsParameters = merge({}, lightsData, lightsParameters);
      this.fogParameters = merge({}, fogData, fogParameters);
      this.fixedViewCameraParameters = merge(
        {},
        cameraData,
        fixedViewCameraParameters
      );
      /*
      this.firstPersonViewCameraParameters = merge(
        {},
        cameraData,
        firstPersonViewCameraParameters
      );
      this.thirdPersonViewCameraParameters = merge(
        {},
        cameraData,
        thirdPersonViewCameraParameters
      );
      */
      this.topViewCameraParameters = merge(
        {},
        cameraData,
        topViewCameraParameters
      );
      this.miniMapCameraParameters = merge(
        {},
        cameraData,
        miniMapCameraParameters
      );
      this.doctorParameters = merge({}, doctorData, doctorParameters);
      this.tablesSurgeryDataParameters = merge(
        {},
        tablesSurgeryData,
        tablesSurgeryDataParameters
      );
      // Create a 2D scene (the viewports frames)
      //this.scene2D = new THREE.Scene();

      // Create a square
      let points = [
        new THREE.Vector3(0.0, 0.0, 0.0),
        new THREE.Vector3(1.0, 0.0, 0.0),
        new THREE.Vector3(1.0, 1.0, 0.0),
        new THREE.Vector3(0.0, 1.0, 0.0),
      ];
      let geometry = new THREE.BufferGeometry().setFromPoints(points);
      const material = new THREE.LineBasicMaterial({ color: 0xffffff });
      this.square = new THREE.LineLoop(geometry, material);
      //this.scene2D.add(this.square);

      /*
      // Create the camera corresponding to the 2D scene
      this.camera2D = new THREE.OrthographicCamera(
        0.0,
        1.0,
        1.0,
        0.0,
        0.0,
        1.0
      );
      */

      // Create a 3D scene (the game itself)
      this.scene3D = new THREE.Scene();
      this.scene3D.background = new THREE.Color(0xc8c8c8); // Set the background color to sky blue

      this.doors = [];

      // Create the maze
      this.maze = new Maze(this.mazeParameters);

      // Create the doors
      this.doorPositionList = [];
      this.doorDirectionList = [];

      //Create the beds

      this.beds = [];
      this.bedPositionList = [];
      this.bedDirectionList = [];

      this.bedWithPatients = [];
      this.bedWithPatientPositionList = [];
      this.bedWithPatientDirectionList = [];

      this.doctors = [];
      this.doctorsPositionList = [];
      this.doctorsDirectionList = [];

      this.surgeryTables = [];
      this.surgeryTablesPositionList = [];
      this.surgeryTablesDirectionList = [];

      for (const room of description.rooms) {
        const table = new SurgeryTable(this.tablesSurgeryDataParameters);
        this.surgeryTables.push(table);
        const tablePosition = this.cellToCartesian(room.tableSurgery.position);
        this.surgeryTablesPositionList.push(tablePosition);
        const tableDirection = room.tableSurgery.direction;

        switch (tableDirection) {
          case "west":
            this.surgeryTablesDirectionList.push(90);
            break;
          case "north":
            this.surgeryTablesDirectionList.push(0);
            break;
          case "south":
            this.surgeryTablesDirectionList.push(180);
            break;
          case "east":
            this.surgeryTablesDirectionList.push(270);
            break;
          default:
            console.log("Direction not handled:", tableDirection);
        }

        let bed1;
        let bed2;
        if (!room.isOccupied) {
          bed1 = new Bed(this.bedParameters);
          this.beds.push(bed1);
          const bedPosition = this.cellToCartesian(room.beds.position);
          const bedDirection = room.beds.direction;
          this.bedPositionList.push(bedPosition);
          switch (bedDirection) {
            case "west":
              this.bedDirectionList.push(90);
              break;
            case "north":
              this.bedDirectionList.push(0);
              break;
            case "south":
              this.bedDirectionList.push(180);
              break;
            case "east":
              this.bedDirectionList.push(270);
              break;

            default:
              console.log("Direction not handled:", bedDirection);
          }
        } else {
          bed2 = new BedWithPatient(this.bedWithPatientParameters);
          this.bedWithPatients.push(bed2);
          const bedWithPatientPosition = this.cellToCartesian(
            room.beds.position
          );
          const bedWithPatientDirection = room.beds.direction;
          this.bedWithPatientPositionList.push(bedWithPatientPosition);

          switch (bedWithPatientDirection) {
            case "west":
              this.bedWithPatientDirectionList.push(90);
              break;
            case "north":
              this.bedWithPatientDirectionList.push(0);
              break;
            case "south":
              this.bedWithPatientDirectionList.push(180);
              break;
            case "east":
              this.bedWithPatientDirectionList.push(270);
              break;
            default:
              console.log("Direction not handled:", bedWithPatientDirection);
          }
        }

        const door = new Door(this.doorParameters);
        this.doors.push(door);
        const doorPosition = this.cellToCartesian(room.doors.position);
        const doorDirection = room.doors.doorDirection;
        this.doorPositionList.push(doorPosition);
        switch (doorDirection) {
          case "west":
            this.doorDirectionList.push(90);
            //console.log("Added door direction: 90 for west");
            break;
          case "north":
            this.doorDirectionList.push(0);
            //console.log("Added door direction: 0 for north");
            break;
          case "south":
            this.doorDirectionList.push(180);
            //console.log("Added door direction: 180 for south");
            break;
          case "east":
            this.doorDirectionList.push(270);
            //console.log("Added door direction: 270 for east");
            break;
          default:
            console.log("Direction not handled:", doorDirection);
        }

        if (room.doctors && room.doctors.length > 0) {
          for (const doctorOfRoom of room.doctors) {
            const doctor = new Doctor(this.doctorParameters);
            this.doctors.push(doctor);
            let doctorPosition;
            let doctorDirection;

            switch (doctorOfRoom.direction) {
              case "west":
                doctorDirection = 90;
                break;
              case "north":
                doctorDirection = 0;
                break;
              case "south":
                doctorDirection = 180;
                break;
              case "east":
                doctorDirection = 270;
                break;
            }
            this.doctorsDirectionList.push(doctorDirection);
            doctorPosition = this.cellToCartesian(doctorOfRoom.position);
            this.doctorsPositionList.push(doctorPosition);
          }
        }
      }

      const canvas = document.getElementById("three-canvas");

      // Create the player
      this.player = new Player(this.playerParameters);

      // Create the lights
      this.lights = new Lights(this.lightsParameters);

      // Create the fog
      this.fog = new Fog(this.fogParameters);

      // Create the cameras corresponding to the four different views: fixed view, first-person view, third-person view and top view
      this.fixedViewCamera = new Camera(
        this.fixedViewCameraParameters,
        canvas.clientWidth,
        canvas.clientHeight
      );
      /*
      this.firstPersonViewCamera = new Camera(
        this.firstPersonViewCameraParameters,
        canvas.clientWidth,
        canvas.clientHeight
      );
      this.thirdPersonViewCamera = new Camera(
        this.thirdPersonViewCameraParameters,
        canvas.clientWidth,
        canvas.clientHeight
      );
      */
      this.topViewCamera = new Camera(
        this.topViewCameraParameters,
        canvas.clientWidth,
        canvas.clientHeight
      );

      // Create the statistics and make its node invisible
      this.statistics = new Stats();
      this.statistics.dom.style.visibility = "hidden";
      document.body.appendChild(this.statistics.dom);

      // Create a renderer and attach it to the canvas element
      this.renderer = new THREE.WebGLRenderer({
        antialias: true,
        canvas: canvas,
      });
      if (this.generalParameters.setDevicePixelRatio) {
        this.renderer.setPixelRatio(window.devicePixelRatio);
      }
      this.renderer.autoClear = false;
      this.renderer.setSize(canvas.clientWidth, canvas.clientHeight);

      // Set the mouse move action (none)
      this.dragMiniMap = false;
      this.changeCameraDistance = false;
      this.changeCameraOrientation = false;

      // Set the game state
      this.gameRunning = false;

      // Get and configure the panel's <div> elements
      this.viewsPanel = document.getElementById("views-panel");
      this.view = document.getElementById("view");
      this.projection = document.getElementById("projection");
      this.horizontal = document.getElementById("horizontal");
      this.horizontal.step = 1;
      this.vertical = document.getElementById("vertical");
      this.vertical.step = 1;
      this.distance = document.getElementById("distance");
      this.distance.step = 0.1;
      this.zoom = document.getElementById("zoom");
      this.zoom.step = 0.1;
      this.reset = document.getElementById("reset");
      this.resetAll = document.getElementById("reset-all");
      //this.helpPanel = document.getElementById("help-panel");
      //this.helpPanel.style.visibility = "hidden";
      //this.subwindowsPanel = document.getElementById("subwindows-panel");
      /*
      this.multipleViewsCheckBox = document.getElementById("multiple-views");
      this.multipleViewsCheckBox.checked = false;
      this.userInterfaceCheckBox = document.getElementById("user-interface");
      this.userInterfaceCheckBox.checked = true;
      this.helpCheckBox = document.getElementById("help");
      this.helpCheckBox.checked = false;
      this.statisticsCheckBox = document.getElementById("statistics");
      this.statisticsCheckBox.checked = false;
      */

      // Build the help panel
      //this.buildHelpPanel();

      // Set the active view camera (fixed view)
      this.setActiveViewCamera(this.fixedViewCamera);

      // Arrange viewports by view mode
      this.arrangeViewports(false);

      // Register the event handler to be called on key down
      document.addEventListener("keydown", (event) =>
        this.keyChange(event, true)
      );

      // Register the event handler to be called on key release
      document.addEventListener("keyup", (event) =>
        this.keyChange(event, false)
      );

      // Register the event handler to be called on mouse down
      this.renderer.domElement.addEventListener("mousedown", (event) =>
        this.mouseDown(event)
      );

      // Register the event handler to be called on mouse move
      this.renderer.domElement.addEventListener("mousemove", (event) =>
        this.mouseMove(event)
      );

      // Register the event handler to be called on mouse up
      this.renderer.domElement.addEventListener("mouseup", (event) =>
        this.mouseUp(event)
      );

      // Register the event handler to be called on mouse wheel
      this.renderer.domElement.addEventListener("wheel", (event) =>
        this.mouseWheel(event)
      );

      // Register the event handler to be called on context menu
      this.renderer.domElement.addEventListener("contextmenu", (event) =>
        this.contextMenu(event)
      );

      // Register the event handler to be called on mouse move
      this.renderer.domElement.addEventListener("mousemove", (event) =>
        this.highlightSurgeryTable(event)
      );

      // Register the event handler to be called on mouse click
      this.renderer.domElement.addEventListener("click", (event) =>
        this.selectRoom(event)
      );

      // Register the event handler to be called on select, input number, or input checkbox change
      this.view.addEventListener("change", (event) =>
        this.elementChange(event)
      );
      this.projection.addEventListener("change", (event) =>
        this.elementChange(event)
      );
      this.horizontal.addEventListener("change", (event) =>
        this.elementChange(event)
      );
      this.vertical.addEventListener("change", (event) =>
        this.elementChange(event)
      );
      this.distance.addEventListener("change", (event) =>
        this.elementChange(event)
      );
      this.zoom.addEventListener("change", (event) =>
        this.elementChange(event)
      );
      /*
      this.multipleViewsCheckBox.addEventListener("change", (event) =>
        this.elementChange(event)
      );
      this.userInterfaceCheckBox.addEventListener("change", (event) =>
        this.elementChange(event)
      );
      this.helpCheckBox.addEventListener("change", (event) =>
        this.elementChange(event)
      );
      this.statisticsCheckBox.addEventListener("change", (event) =>
        this.elementChange(event)
      );
      */

      // Register the event handler to be called on input button click
      this.reset.addEventListener("click", (event) => this.buttonClick(event));
      this.resetAll.addEventListener("click", (event) =>
        this.buttonClick(event)
      );

      this.activeElement = document.activeElement;
    };
    console.log("Create scene");
    this.onProgress = function (url, xhr) {
      /*console.log(
        "Resource '" +
          url +
          "' " +
          ((100.0 * xhr.loaded) / xhr.total).toFixed(0) +
          "% loaded."
      );
      */
    };

    this.onError = function (url, error) {
      console.error("Error loading resource " + url + " (" + error + ").");
    };

    for (const [key, value] of Object.entries(parameters)) {
      this[key] = value;
    }
    this.loaded = false;

    // The cache must be enabled; additional information available at https://threejs.org/docs/api/en/loaders/FileLoader.html
    THREE.Cache.enabled = true;

    // Create a resource file loader
    const loader = new THREE.FileLoader();

    // Set the response type: the resource file will be parsed with JSON.parse()
    loader.setResponseType("json");

    // Load a maze description resource file
    loader.load(
      //Resource URL
      this.url,

      // onLoad callback
      (description) => this.onLoad(description),

      // onProgress callback
      (xhr) => this.onProgress(this.url, xhr),

      // onError callback
      (error) => this.onError(this.url, error)
    );
  }

  buildHelpPanel() {
    const table = document.getElementById("help-table");
    let i = 0;
    for (const key in this.player.keyCodes) {
      while (table.rows[i].cells.length < 2) {
        i++;
      }
      table.rows[i++].cells[0].innerHTML = this.player.keyCodes[key];
    }
    table.rows[i].cells[0].innerHTML =
      this.maze.credits + "<br>" + this.player.credits;
  }

  displayPanel() {
    this.view.options.selectedIndex = [
      "fixed",
      //"first-person",
      //"third-person",
      "top",
    ].indexOf(this.activeViewCamera.view);
    this.projection.options.selectedIndex = [
      "perspective",
      "orthographic",
    ].indexOf(this.activeViewCamera.projection);
    this.horizontal.value = this.activeViewCamera.orientation.h.toFixed(0);
    this.vertical.value = this.activeViewCamera.orientation.v.toFixed(0);
    this.distance.value = this.activeViewCamera.distance.toFixed(1);
    this.zoom.value = this.activeViewCamera.zoom.toFixed(1);
  }

  // Set active view camera
  setActiveViewCamera(camera) {
    this.activeViewCamera = camera;
    this.horizontal.min = this.activeViewCamera.orientationMin.h.toFixed(0);
    this.horizontal.max = this.activeViewCamera.orientationMax.h.toFixed(0);
    this.vertical.min = this.activeViewCamera.orientationMin.v.toFixed(0);
    this.vertical.max = this.activeViewCamera.orientationMax.v.toFixed(0);
    this.distance.min = this.activeViewCamera.distanceMin.toFixed(1);
    this.distance.max = this.activeViewCamera.distanceMax.toFixed(1);
    this.zoom.min = this.activeViewCamera.zoomMin.toFixed(1);
    this.zoom.max = this.activeViewCamera.zoomMax.toFixed(1);
    this.displayPanel();
  }

  arrangeViewports(multipleViews) {
    this.fixedViewCamera.setViewport(multipleViews);
    //this.firstPersonViewCamera.setViewport(multipleViews);
    //this.thirdPersonViewCamera.setViewport(multipleViews);
    this.topViewCamera.setViewport(multipleViews);
  }

  pointerIsOverViewport(pointer, viewport) {
    return (
      pointer.x >= viewport.x &&
      pointer.x < viewport.x + viewport.width &&
      pointer.y >= viewport.y &&
      pointer.y < viewport.y + viewport.height
    );
  }

  getPointedViewport(pointer) {
    let viewport;
    // Check if the pointer is over the remaining camera viewports
    let cameras;
    /*if (this.multipleViewsCheckBox.checked) {
      cameras = [
        this.fixedViewCamera,
        //this.firstPersonViewCamera,
        //this.thirdPersonViewCamera,
        this.topViewCamera,
      ];
    } else {*/
    cameras = [this.activeViewCamera];
    //}
    for (const camera of cameras) {
      viewport = camera.getViewport();
      if (this.pointerIsOverViewport(pointer, viewport)) {
        return camera.view;
      }
    }
    // No camera viewport is being pointed
    return "none";
  }

  setViewMode(multipleViews) {
    // Single-view mode: false; multiple-views mode: true
    this.multipleViewsCheckBox.checked = multipleViews;
    this.arrangeViewports(this.multipleViewsCheckBox.checked);
  }

  setUserInterfaceVisibility(visible) {
    this.userInterfaceCheckBox.checked = visible;
    this.viewsPanel.style.visibility = visible ? "visible" : "hidden";
    //this.subwindowsPanel.style.visibility = visible ? "visible" : "hidden";
    this.userInterface.setVisibility(visible);
  }

  setMiniMapVisibility(visible) {
    // Hidden: false; visible: true
    this.miniMapCheckBox.checked = visible;
  }

  cellToCartesian(position) {
    return new THREE.Vector3(
      (position[1] - this.size.width / 2.0 + 0.5) * this.scale.x,
      0.0,
      (position[0] - this.size.height / 2.0 + 0.5) * this.scale.z
    );
  }

  setHelpVisibility(visible) {
    // Hidden: false; visible: true
    this.helpCheckBox.checked = visible;
    this.helpPanel.style.visibility = "hidden";
  }

  setStatisticsVisibility(visible) {
    // Hidden: false; visible: true
    this.statisticsCheckBox.checked = visible;
    this.statistics.dom.style.visibility = visible ? "visible" : "hidden";
  }

  windowResize() {
    this.fixedViewCamera.updateWindowSize(
      window.innerWidth,
      window.innerHeight
    );
    /*this.firstPersonViewCamera.updateWindowSize(
      window.innerWidth,
      window.innerHeight
    );
    this.thirdPersonViewCamera.updateWindowSize(
      window.innerWidth,
      window.innerHeight
    );*/
    this.topViewCamera.updateWindowSize(window.innerWidth, window.innerHeight);
    this.miniMapCamera.updateWindowSize(window.innerWidth, window.innerHeight);
    this.renderer.setSize(window.innerWidth, window.innerHeight);
  }

  keyChange(event, state) {
    // Allow digit and arrow keys to be used when entering numbers
    if (
      ["horizontal", "vertical", "distance", "zoom"].indexOf(event.target.id) <
      0
    ) {
      event.target.blur();
    }
    if (document.activeElement == document.body) {
      // Prevent the "Space" and "Arrow" keys from scrolling the document's content
      if (
        event.code == "Space" ||
        event.code == "ArrowLeft" ||
        event.code == "ArrowRight" ||
        event.code == "ArrowDown" ||
        event.code == "ArrowUp"
      ) {
        event.preventDefault();
      }
      if (event.code == this.player.keyCodes.fixedView && state) {
        // Select fixed view
        this.setActiveViewCamera(this.fixedViewCamera);
        /*} else if (event.code == this.player.keyCodes.firstPersonView && state) {
        // Select first-person view
        this.setActiveViewCamera(this.firstPersonViewCamera);
      } else if (event.code == this.player.keyCodes.thirdPersonView && state) {
        // Select third-person view
        this.setActiveViewCamera(this.thirdPersonViewCamera);
        */
      } else if (event.code == this.player.keyCodes.topView && state) {
        // Select top view
        this.setActiveViewCamera(this.topViewCamera);
      }
      if (event.code == this.player.keyCodes.viewMode && state) {
        // Single-view mode / multiple-views mode
        this.setViewMode(!this.multipleViewsCheckBox.checked);
      }
      /* To-do #41 - Toggle the user interface visibility
                - event code: this.player.keyCodes.userInterface
                - state: true
            if (... && ...) { // Display / hide user interface
                this.setUserInterfaceVisibility(!this.userInterfaceCheckBox.checked);
            } */

      if (event.code == this.player.keyCodes.help && state) {
        // Display / hide help
        this.setHelpVisibility(!this.helpCheckBox.checked);
      }
      if (event.code == this.player.keyCodes.statistics && state) {
        // Display / hide statistics
        this.setStatisticsVisibility(!this.statisticsCheckBox.checked);
      }
      if (event.code == this.player.keyCodes.run) {
        this.player.keyStates.run = state;
      }
      if (event.code == this.player.keyCodes.left) {
        this.player.keyStates.left = state;
      } else if (event.code == this.player.keyCodes.right) {
        this.player.keyStates.right = state;
      }
      if (event.code == this.player.keyCodes.backward) {
        this.player.keyStates.backward = state;
      } else if (event.code == this.player.keyCodes.forward) {
        this.player.keyStates.forward = state;
      }
      if (event.code == this.player.keyCodes.jump) {
        this.player.keyStates.jump = state;
      } else if (event.code == this.player.keyCodes.yes) {
        this.player.keyStates.yes = state;
      } else if (event.code == this.player.keyCodes.no) {
        this.player.keyStates.no = state;
      } else if (event.code == this.player.keyCodes.wave) {
        this.player.keyStates.wave = state;
      } else if (event.code == this.player.keyCodes.punch) {
        this.player.keyStates.punch = state;
      } else if (event.code == this.player.keyCodes.thumbsUp) {
        this.player.keyStates.thumbsUp = state;
      }
    }
  }

  highlightSurgeryTable(event) {
    
    if (event.type === "click") {
      // Calculate mouse position in normalized device coordinates (-1 to +1) for both components
      const rect = this.renderer.domElement.getBoundingClientRect();
      const mouse = new THREE.Vector2(
        ((event.clientX - rect.left) / rect.width) * 2 - 1,
        -((event.clientY - rect.top) / rect.height) * 2 + 1
      );
      

      // Create a raycaster
      const raycaster = new THREE.Raycaster();
      raycaster.setFromCamera(mouse, this.activeViewCamera.object);

      // Check for intersections with surgical tables (or beds in this case)
      const validObjects = this.beds
        .map((table) => table.object)
        .filter((object) => object !== undefined && object !== null);

      const intersects = raycaster.intersectObjects(validObjects, true);
      
      if (intersects.length > 0) {
        const target = intersects[0].point;
        if (this.isZoomedIn) {
          // Zooming out: restore the original camera position
          console.log("restore");
          GSAP.gsap.to(this.activeViewCamera.object.position, {
            duration: 1,
            x: this.originalCameraPosition.x,
            y: this.originalCameraPosition.y,
            z: this.originalCameraPosition.z,
            ease: "power2.inOut",
            onUpdate: () => {
              this.activeViewCamera.object.lookAt(target); // Ensure the camera keeps looking at the target
            },
            onComplete: () => {
              this.isZoomedIn = false; // Mark as zoomed out
            },
          });
        } else {
          // Zooming in: store original position and move camera to target
          this.originalCameraPosition =
            this.activeViewCamera.object.position.clone();

          GSAP.gsap.to(this.activeViewCamera.object.position, {
            duration: 1,
            x: target.x,
            y: target.y + 1, // Slightly above the target for better visibility
            z: target.z + 2, // Move back a little to avoid clipping
            ease: "power2.inOut",
            onUpdate: () => {
              this.activeViewCamera.object.lookAt(target); // Focus on the target point
              this.isZoomedIn = true; // Mark as zoomed in
            },
            onComplete: () => {
              this.isZoomedIn = true; // Mark as zoomed in
            },
          });
        }
      }
    }
  }

  mouseDown(event) {
    if (event.buttons == 1 || event.buttons == 2) {
      // Primary or secondary button down
      // Store current mouse position in window coordinates (mouse coordinate system: origin in the top-left corner; window coordinate system: origin in the bottom-left corner)
      this.mousePosition = new THREE.Vector2(
        event.clientX,
        window.innerHeight - event.clientY - 1
      );
      // Select the camera whose view is being pointed
      const cameraView = this.getPointedViewport(this.mousePosition);
      if (cameraView != "none") {
        if (event.buttons == 1) {
          // Primary button down
          this.changeCameraDistance = true;
        } else {
          // Secondary button down
          this.changeCameraOrientation = true;
        }
      }
    }
  }

  mouseMove(event) {
    if (event.buttons == 1 || event.buttons == 2) {
      // Primary or secondary button down
      if (
        this.changeCameraDistance ||
        this.changeCameraOrientation ||
        this.dragMiniMap
      ) {
        // Mouse action in progress
        // Compute mouse movement and update mouse position
        const newMousePosition = new THREE.Vector2(
          event.clientX,
          window.innerHeight - event.clientY - 1
        );
        const mouseIncrement = newMousePosition.clone().sub(this.mousePosition);
        this.mousePosition = newMousePosition;
        if (event.buttons == 1) {
          // Primary button down
          if (this.changeCameraDistance) {
            this.activeViewCamera.updateDistance(
              -0.05 * (mouseIncrement.x + mouseIncrement.y)
            );
            this.displayPanel();
          } else if (this.dragMiniMap) {
            const windowMinSize = Math.min(
              window.innerWidth,
              window.innerHeight
            );
            const width = this.miniMapCamera.viewport.width * windowMinSize;
            const height = this.miniMapCamera.viewport.height * windowMinSize;
            this.miniMapCamera.viewport.x +=
              mouseIncrement.x / (window.innerWidth - width);
            this.miniMapCamera.viewport.y +=
              mouseIncrement.y / (window.innerHeight - height);
          }
        } else {
          // Secondary button down
          if (this.changeCameraOrientation) {
            this.activeViewCamera.updateOrientation(
              mouseIncrement.multiply(new THREE.Vector2(-0.5, 0.5))
            );
            this.displayPanel();
          }
        }
      }
    }
  }

  mouseUp(event) {
    // Reset mouse move action
    this.dragMiniMap = false;
    this.changeCameraDistance = false;
    this.changeCameraOrientation = false;
  }

  mouseWheel(event) {
    // Prevent the mouse wheel from scrolling the document's content
    event.preventDefault();
    // Store current mouse position in window coordinates (mouse coordinate system: origin in the top-left corner; window coordinate system: origin in the bottom-left corner)
    this.mousePosition = new THREE.Vector2(
      event.clientX,
      window.innerHeight - event.clientY - 1
    );
    // Select the camera whose view is being pointed
    const cameraView = this.getPointedViewport(this.mousePosition);
    if (cameraView != "none") {
      // One of the remaining cameras selected
      const cameraIndex = [
        "fixed",
        //"first-person",
        //"third-person",
        "top",
      ].indexOf(cameraView);
      this.view.options.selectedIndex = cameraIndex;
      const activeViewCamera = [
        this.fixedViewCamera,
        //this.firstPersonViewCamera,
        //this.thirdPersonViewCamera,
        this.topViewCamera,
      ][cameraIndex];
      activeViewCamera.updateZoom(-0.001 * event.deltaY);
      this.setActiveViewCamera(activeViewCamera);
    }
  }

  selectRoom(event) {
    // Calculate mouse position in normalized device coordinates (-1 to +1) for both components
    const rect = this.renderer.domElement.getBoundingClientRect();
    const mouse = new THREE.Vector2(
      ((event.clientX - rect.left) / rect.width) * 2 - 1,
      -((event.clientY - rect.top) / rect.height) * 2 + 1
    );

    // Create a raycaster
    const raycaster = new THREE.Raycaster();
    raycaster.setFromCamera(mouse, this.activeViewCamera.object);

    // Check for intersections with beds
    const intersects = raycaster.intersectObjects(
      this.beds.map((bed) => bed.object),
      true
    );

    if (intersects.length > 0) {
      // Find the corresponding bed
      for (const bed of this.beds) {
        if (bed.object.getObjectById(intersects[0].object.id)) {
          const roomCenter = bed.position;
          const target = new THREE.Vector3(
            roomCenter.x,
            roomCenter.y,
            roomCenter.z
          );
          GSAP.gsap.to(this.activeViewCamera.object.position, {
            duration: 1,
            x: target.x,
            z: target.z,
            ease: "power2.inOut",
            onUpdate: () => {
              this.activeViewCamera.object.lookAt(target);
            },
          });
          break;
        }
      }
    }
  }

  contextMenu(event) {
    // Prevent the context menu from appearing when the secondary mouse button is clicked
    event.preventDefault();
  }

  elementChange(event) {
    switch (event.target.id) {
      case "view":
        this.setActiveViewCamera(
          [
            this.fixedViewCamera,
            //this.firstPersonViewCamera,
            //this.thirdPersonViewCamera,
            this.topViewCamera,
          ][this.view.options.selectedIndex]
        );
        break;
      case "projection":
        this.activeViewCamera.setActiveProjection(
          ["perspective", "orthographic"][this.projection.options.selectedIndex]
        );
        this.displayPanel();
        break;
      case "horizontal":
      case "vertical":
      case "distance":
      case "zoom":
        if (event.target.checkValidity()) {
          switch (event.target.id) {
            case "horizontal":
            case "vertical":
              this.activeViewCamera.setOrientation(
                new Orientation(this.horizontal.value, this.vertical.value)
              );
              break;
            case "distance":
              this.activeViewCamera.setDistance(this.distance.value);
              break;
            case "zoom":
              this.activeViewCamera.setZoom(this.zoom.value);
              break;
          }
        }
        break;
      case "multiple-views":
        this.setViewMode(event.target.checked);
        break;
      case "user-interface":
        this.setUserInterfaceVisibility(event.target.checked);
        break;
      case "help":
        this.setHelpVisibility(event.target.checked);
        break;
      case "statistics":
        this.setStatisticsVisibility(event.target.checked);
        break;
    }
  }

  buttonClick(event) {
    switch (event.target.id) {
      case "reset":
        this.activeViewCamera.initialize();
        break;
      case "reset-all":
        this.fixedViewCamera.initialize();
        //this.firstPersonViewCamera.initialize();
        //this.thirdPersonViewCamera.initialize();
        this.topViewCamera.initialize();
        break;
    }
    this.displayPanel();
  }

  finalSequence() {
    /* To-do #43 - Trigger the final sequence
            1 - Disable the fog
            2 - Reconfigure the third-person view camera:
                - horizontal orientation: -180.0
                - vertical orientation: this.thirdPersonViewCamera.initialOrientation.v
                - distance: this.thirdPersonViewCamera.initialDistance
                - zoom factor: 2.0
            3 - Set it as the active view camera
            4 - Set single-view mode:
                - false: single-view
                - true: multiple-views
            5 - Set the final action:
                - action: "Dance"
                - duration: 0.2 seconds
        this.fog.enabled = ...;
        this.thirdPersonViewCamera.setOrientation(new Orientation(..., ...));
        this.thirdPersonViewCamera.setDistance(...);
        this.thirdPersonViewCamera.setZoom(...);
        this.setActiveViewCamera(...);
        this.setViewMode(...);
        this.animations.fadeToAction(..., ...); */
  }

  collision(position) {
    /* To-do #24 - Check if the player collided with a wall
            - assume that a collision is detected if the distance between the player position and any of the walls is less than the player radius.
            - player position: position
            - player radius: this.player.radius
            - remove the previous instruction and replace it with the following one (after completing it)
        return this.maze.distanceToWestWall(position) < ... || ... || ... || ...; */

    return (
      this.maze.distanceToWestWall(position) < this.player.radius ||
      this.maze.distanceToEastWall(position) < this.player.radius ||
      this.maze.distanceToNorthWall(position) < this.player.radius ||
      this.maze.distanceToSouthWall(position) < this.player.radius
    );
  }

  async updateRoomOccupancy(apiUrl) {
    try {
      const data = apiUrl;
      console.log(data);

      // Check if there are any appointments happening
      if (data.length === 0) {
        // If no appointments are happening, set all rooms as unoccupied
        rooms.forEach((room) => {
          room.isOccupied = false;
        });
      } else {
        // If there are appointments happening, update the occupancy based on the appointments
        rooms.forEach((room) => {
          // Find if there's an appointment for this room
          const roomData = data.find(
            (apiRoom) => apiRoom.roomNumber === room.name
          );
          if (roomData) {
            room.isOccupied = true;
          } else {
            room.isOccupied = false;
          }
        });
      }
    } catch (error) {
      console.error("Error fetching room occupancy data:", error);
    }
  }

  update() {
    if (!this.gameRunning) {
      const allDoorsLoaded =
        this.doors && this.doors.every((door) => door && door.loaded);
      const allBedsLoaded =
        this.beds &&
        this.beds.length > 0 &&
        this.beds.every((bed) => bed && bed.loaded);
      const allBedsOccupiedLoaded =
        this.bedWithPatients &&
        this.bedWithPatients.every(
          (bedPatient) => bedPatient && bedPatient.loaded
        );

      const allDoctorsLoaded =
        //this.doctors.length > 0 &&
        this.doctors && this.doctors.every((doctor) => doctor && doctor.loaded);
      const allSurgeryTablesLoaded =
        this.surgeryTables &&
        this.surgeryTables.length > 0 &&
        this.surgeryTables.every((table) => table && table.loaded);

      //const allSurgeryTablesLoaded = this.surgeryTables && this.surgeryTables.length > 0 && this.surgeryTables.every((table) => table && table.loaded);

      if (
        this.maze &&
        this.player &&
        this.maze.loaded &&
        this.player.loaded &&
        allDoorsLoaded &&
        allBedsLoaded &&
        allDoctorsLoaded &&
        allBedsOccupiedLoaded &&
        allSurgeryTablesLoaded
      ) {
        // If all resources have been loaded
        // Add the maze, the player and the lights to the scene
        this.scene3D.add(this.maze.object);
        //this.scene3D.add(this.player.object);
        this.scene3D.add(this.lights.object);

        // Create the clock
        this.clock = new THREE.Clock();

        // Create model animations (states, emotes and expressions)
        this.animations = new Animations(
          this.player.object,
          this.player.animations
        );

        // Set the player's position and direction
        this.player.position = this.maze.initialPosition.clone();
        this.player.direction = this.maze.initialDirection;

        for (let i = 0; i < this.doors.length; i++) {
          this.doors[i].position = this.doorPositionList[i].clone();
          this.doors[i].doorDirection = THREE.MathUtils.degToRad(
            this.doorDirectionList[i]
          );
        }

        this.doors.forEach((door) => {
          //console.log("Esta a adicionar a porta a cena: ");
          this.scene3D.add(door.object);
        });

        this.doors.forEach((door) => {
          door.object.position.set(
            door.position.x - 0.51,
            door.position.y,
            door.position.z
          );
        });

        this.doors.forEach((door) => {
          door.object.rotation.y = door.doorDirection;
        });

        for (let i = 0; i < this.beds.length; i++) {
          this.beds[i].position = this.bedPositionList[i].clone();
          this.beds[i].direction = THREE.MathUtils.degToRad(
            this.bedDirectionList[i]
          );
          this.scene3D.add(this.beds[i].object);
          this.beds[i].object.position.set(
            this.beds[i].position.x - 0.5,
            this.beds[i].position.y + 0.291,
            this.beds[i].position.z
          );
          this.beds[i].object.rotation.y = this.beds[i].direction;
        }

        for (let i = 0; i < this.bedWithPatients.length; i++) {
          this.bedWithPatients[i].position =
            this.bedWithPatientPositionList[i].clone();
          this.bedWithPatients[i].direction = THREE.MathUtils.degToRad(
            this.bedWithPatientDirectionList[i]
          );
          this.scene3D.add(this.bedWithPatients[i].object);
          this.bedWithPatients[i].object.position.set(
            this.bedWithPatients[i].position.x - 0.55,
            this.bedWithPatients[i].position.y + 0.344,
            this.bedWithPatients[i].position.z
          );
          this.bedWithPatients[i].object.rotation.y =
            this.bedWithPatients[i].direction;
        }

        for (let i = 0; i < this.doctors.length; i++) {
          this.doctors[i].position = this.doctorsPositionList[i].clone();
          this.doctors[i].direction = THREE.MathUtils.degToRad(
            this.doctorsDirectionList[i]
          );
          this.scene3D.add(this.doctors[i].object);
          this.doctors[i].object.position.set(
            this.doctors[i].position.x - 1,
            this.doctors[i].position.y + 0.58,
            this.doctors[i].position.z - 2.5
          );
          this.doctors[i].object.rotation.y = this.doctors[i].direction;
        }

        for (let i = 0; i < this.surgeryTables.length; i++) {
          this.surgeryTables[i].position =
            this.surgeryTablesPositionList[i].clone();
          this.surgeryTables[i].direction = THREE.MathUtils.degToRad(
            this.surgeryTablesDirectionList[i]
          );
          this.scene3D.add(this.surgeryTables[i].object);
          this.surgeryTables[i].object.position.set(
            this.surgeryTables[i].position.x - 0.5,
            this.surgeryTables[i].position.y,
            this.surgeryTables[i].position.z
          );
          this.surgeryTables[i].object.rotation.y =
            this.surgeryTables[i].direction;
        }

        /* To-do #40 - Create the user interface
                    - parameters: this.scene3D, this.renderer, this.lights, this.fog, this.player.object, this.animations
                this.userInterface = new UserInterface(...); */

        // Start the game
        this.gameRunning = true;
      }
    } else {
      // Update the model animations
      const deltaT = this.clock.getDelta();
      this.animations.update(deltaT);

      // Update the player
      if (!this.animations.actionInProgress) {
        // Check if the player found the exit
        if (this.maze.foundExit(this.player.position)) {
          this.finalSequence();
        } else {
          let coveredDistance = this.player.walkingSpeed * deltaT;
          let directionIncrement = this.player.turningSpeed * deltaT;

          if (this.player.keyStates.run) {
            coveredDistance *= this.player.runningFactor;
            directionIncrement *= this.player.runningFactor;
          }
          if (this.player.keyStates.left) {
            this.player.direction += directionIncrement;
          } else if (this.player.keyStates.right) {
            this.player.direction -= directionIncrement;
          }

          const direction = THREE.MathUtils.degToRad(this.player.direction);
          if (this.player.keyStates.backward) {
            const newPosition = new THREE.Vector3(
              -coveredDistance * Math.sin(direction),
              0.0,
              -coveredDistance * Math.cos(direction)
            ).add(this.player.position);
            if (this.collision(newPosition)) {
              this.animations.fadeToAction("Death", 0.2);
            } else {
              this.animations.fadeToAction(
                this.player.keyStates.run ? "Running" : "Walking",
                0.2
              );
              this.player.position = newPosition;
            }
          } else if (this.player.keyStates.forward) {
            // The player is moving forward
            const newPosition = new THREE.Vector3(
              coveredDistance * Math.sin(direction),
              0.0,
              coveredDistance * Math.cos(direction)
            ).add(this.player.position);
            if (this.collision(newPosition)) {
              this.animations.fadeToAction("Death", 0.2);
            } else {
              this.animations.fadeToAction(
                this.player.keyStates.run ? "Running" : "Walking",
                0.2
              );
              this.player.position = newPosition;
            }
          } else if (this.player.keyStates.jump) {
            this.animations.fadeToAction("Jump", 0.2);
          } else if (this.player.keyStates.yes) {
            this.animations.fadeToAction("Yes", 0.2);
          } else if (this.player.keyStates.no) {
            this.animations.fadeToAction("No", 0.2);
          } else if (this.player.keyStates.wave) {
            this.animations.fadeToAction("Wave", 0.2);
          } else if (this.player.keyStates.punch) {
            this.animations.fadeToAction("Punch", 0.2);
          } else if (this.player.keyStates.thumbsUp) {
            this.animations.fadeToAction("ThumbsUp", 0.2);
          } else {
            this.animations.fadeToAction(
              "Idle",
              this.animations.activeName != "Death" ? 0.2 : 0.6
            );
          }

          this.player.object.position.set(
            this.player.position.x,
            this.player.position.y,
            this.player.position.z
          );
          this.player.object.rotation.y =
            direction - this.player.initialDirection;
        }
      }

      // Update first-person, third-person and top view cameras parameters (player direction and target)
      //this.firstPersonViewCamera.playerDirection = this.player.direction;
      //this.thirdPersonViewCamera.playerDirection = this.player.direction;
      this.topViewCamera.playerDirection = this.player.direction;
      const target = new THREE.Vector3(
        this.player.position.x,
        this.player.position.y + this.player.eyeHeight,
        this.player.position.z
      );
      //this.firstPersonViewCamera.setTarget(target);
      //this.thirdPersonViewCamera.setTarget(target);
      this.topViewCamera.setTarget(target);

      // Update statistics
      this.statistics.update();

      // Render primary viewport(s)
      this.renderer.clear();

      /* To-do #39 - If the fog is enabled, then assign it to the scene; else, assign null
                - fog enabled: this.fog.enabled
                - fog: this.fog.object
            if (...) {
                this.scene3D... = ...;
            }
            else {
                this.scene3D... = ...;
            } */
      let cameras;
      /*if (this.multipleViewsCheckBox.checked) {
        cameras = [
          this.fixedViewCamera,
          //this.firstPersonViewCamera,
          //this.thirdPersonViewCamera,
          this.topViewCamera,
        ];
      } else { */
      cameras = [this.activeViewCamera];
      //}
      for (const camera of cameras) {
        //this.player.object.visible = camera != this.firstPersonViewCamera;
        const viewport = camera.getViewport();
        this.renderer.setViewport(
          viewport.x,
          viewport.y,
          viewport.width,
          viewport.height
        );
        this.renderer.render(this.scene3D, camera.object);
        //this.renderer.render(this.scene2D, this.camera2D);
        //this.renderer.clearDepth();
      }
    }
  }
}
