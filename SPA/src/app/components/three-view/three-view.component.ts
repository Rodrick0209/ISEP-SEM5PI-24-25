import { AfterViewInit, Component, OnInit } from '@angular/core';
import * as THREE from 'three';
import TWEEN from '@tweenjs/tween.js';
import { merge } from './assets/merge';
import ThumbRaiser from './assets/thumb_raiser_template';
import { mazeData } from './assets/default_data';
import { HttpClient } from '@angular/common/http'; // Import HttpClient
import { FormsModule } from '@angular/forms';

@Component({
  selector: 'app-three-view',
  standalone: true,
  imports: [FormsModule],
  templateUrl: './three-view.component.html',
  styleUrls: ['./three-view.component.css']
})
export class ThreeViewComponent implements AfterViewInit, OnInit {
  thumbRaiser: any;
  currentDate: string;
  currentTime: string;
  previousTime: string;

  constructor(private http: HttpClient) {
    const now = new Date();
    this.currentDate = now.toISOString().split('T')[0];
    this.currentTime = now.toTimeString().split(' ')[0].substring(0, 5);
    this.previousTime = this.currentTime;
  }

  ngOnInit(): void {
    // Set an interval to check for time change every second
    setInterval(() => {
      const now = new Date();
      const newTime = now.toTimeString().split(' ')[0].substring(0, 5);
      
      // If the time has changed, call the search method
      if (newTime !== this.previousTime) {
        this.previousTime = newTime;
        this.search();
      }
    }, 2000); // Check every second
  }

  ngAfterViewInit(): void {
    this.initializeGame(this.currentDate, this.currentTime);
    this.animate();
  }

  initializeGame(date: string, time: string): void {
    const parameters = merge({}, mazeData, { scale: new THREE.Vector3(1.0, 0.5, 1.0) });
    this.thumbRaiser = new ThumbRaiser(
      parameters, // Parameters
      {}, // General Parameters
      { scale: new THREE.Vector3(1.0, 0.5, 1.0) }, // Maze parameters
      {}, // Player parameters
      { ambientLight: { intensity: 0.5 }, 
        pointLight1: { intensity: 70.0, distance: 20.0, position: new THREE.Vector3(-3.5, 10.0, 2.5) },
        pointLight2: { intensity: 70.0, distance: 20.0, position: new THREE.Vector3(3.5, 10.0, -2.5) }
      }, // Lights parameters
      {}, // Fog parameters
      {}, // Fixed view camera parameters
      {}, // Top view camera parameters
      {}, // Mini-map camera parameters
      {}, // Door parameters
      {}, // Bed parameters
      {}, // Doctor parameters
      {}, // Bed with patient parameters
      {}, // Tables surgery data parameters
      { date: date }, // Date
      { time: time } // Time
    );
    console.log(date);
    console.log(time);
  }

  animate(): void {
    const updateFrame = () => {
      requestAnimationFrame(updateFrame);
      this.thumbRaiser?.update();
    };
    updateFrame();
  }

  search(): void {
    this.initializeGame(this.currentDate, this.currentTime);
  }
}