import { CommonModule } from '@angular/common';
import { Component } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { Router } from '@angular/router';
import { MedicalCondition } from '../../models/medicalCondition';
import { MedicalCondtionService } from '../../services/medicalConditions.service';
import { MedicalConditionCatalog } from '../../models/medicalConditionCatalog';
import { MessageService } from '../../services/message.service';



@Component({
  selector: 'app-list-medical-conditions',
  standalone: true,
  imports: [CommonModule,FormsModule],
  templateUrl: './list-medical-conditions.component.html',
  styleUrl: './list-medical-conditions.component.css'
})
export class ListMedicalConditionsComponent {
    medicalConditions: MedicalConditionCatalog[] = [];
    isLoading = false;
    sucessMessage: string = '';
    message: string = '';

    constructor(private router: Router, private medicalConditionsService: MedicalCondtionService, private messageService: MessageService) { }

    ngOnInit(): void {
      this.sucessMessage = this.messageService.getMessage() ?? '';
      this.message = '';
      this.isLoading = true;
      this.medicalConditionsService.getMedicalConditions().subscribe({
        next: (data: MedicalConditionCatalog[]) => {
          this.isLoading = false;
          this.medicalConditions = data;
        },
        error: (err: any) => {
          console.error('Failed to fetch medical conditions', err);
          this.isLoading = false;
          this.medicalConditions = [];
          this.message = 'An error occurred while fetching medical conditions: ';
        }
      });
    }




    addMedicalCondition() {
      this.router.navigate(['/medicalConditions/add']);
    }

    editMedicalCondition(medicalCondition: MedicalConditionCatalog) {
      this.router.navigate(['/medicalConditions/edit', medicalCondition.code]);
    }

}
