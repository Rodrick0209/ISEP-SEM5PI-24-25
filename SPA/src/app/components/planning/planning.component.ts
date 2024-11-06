import { Component } from '@angular/core';
import { PlanningService } from '../../services/planning.service';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';

@Component({
  selector: 'app-planning',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './planning.component.html',
  styleUrl: './planning.component.css'
})
export class PlanningComponent {
  greetingMessage: string = '';
  name: string = '';

  constructor(private planningService: PlanningService) { }

  greet() {
    this.planningService.greet(this.name).subscribe(
      (response: any) => {
        this.greetingMessage = response.message; // Extract the message from the response
      },
      error => {
        this.greetingMessage = 'An error occurred while greeting the user: ' + error.message;
      }
    );
  }
}
