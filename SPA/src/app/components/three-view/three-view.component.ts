import { AfterViewInit, Component } from '@angular/core';
import * as THREE from 'three';
import { merge } from './assets/merge';
import Orientation from './assets/orientation';
import ThumbRaiser from './assets/thumb_raiser_template';
import { mazeData } from './assets/default_data';


@Component({
  selector: 'app-three-view',
  standalone: true,
  imports: [],
  templateUrl: './three-view.component.html',
  styleUrl: './three-view.component.css'
})
export class ThreeViewComponent implements AfterViewInit {

  thumbRaiser: any;

  ngAfterViewInit(): void {
    this.initializeGame();
    this.animate();
  }

  initializeGame(): void {
    const parameters = merge({}, mazeData, { scale: new THREE.Vector3(1.0, 0.5, 1.0) });

    this.thumbRaiser = new ThumbRaiser(
      parameters, // Parameters
      {}, // General Parameters
      { scale: new THREE.Vector3(1.0, 0.5, 1.0) }, // Maze parameters
      {}, // Player parameters
      { ambientLight: { intensity: 0.1 }, pointLight1: { intensity: 50.0, distance: 20.0, position: new THREE.Vector3(-3.5, 10.0, 2.5) }, pointLight2: { intensity: 50.0, distance: 20.0, position: new THREE.Vector3(3.5, 10.0, -2.5) } }, // Lights parameters
      { view: 'fixed', multipleViewsViewport: new THREE.Vector4(0.0, 1.0, 0.45, 0.5) }, // Fixed view camera parameters
      { view: 'first-person', multipleViewsViewport: new THREE.Vector4(1.0, 1.0, 0.55, 0.5), initialOrientation: new Orientation(0.0, -10.0), initialDistance: 2.0, distanceMin: 1.0, distanceMax: 4.0 }, // First-person view camera parameters
      { view: 'third-person', multipleViewsViewport: new THREE.Vector4(0.0, 0.0, 0.55, 0.5), initialOrientation: new Orientation(0.0, -20.0), initialDistance: 2.0, distanceMin: 1.0, distanceMax: 4.0 }, // Third-person view camera parameters
      { view: 'top', multipleViewsViewport: new THREE.Vector4(1.0, 0.0, 0.45, 0.5), initialOrientation: new Orientation(0.0, -90.0), initialDistance: 4.0, distanceMin: 1.0, distanceMax: 16.0 } // Top view camera parameters
    );
  }

  animate(): void {
    const updateFrame = () => {
      requestAnimationFrame(updateFrame);
      this.thumbRaiser?.update();
    };

    updateFrame();
  }
}
