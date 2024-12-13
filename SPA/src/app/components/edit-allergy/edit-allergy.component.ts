import { CommonModule } from '@angular/common';
import { Component } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { AllergyCatalogService } from '../../services/allergiesCatalog.service';
import { ActivatedRoute, Router } from '@angular/router';
import { MessageService } from '../../services/message.service';

@Component({
  selector: 'app-edit-allergy',
  standalone: true,
  imports: [FormsModule, CommonModule],
  templateUrl: './edit-allergy.component.html',
  styleUrl: './edit-allergy.component.css'
})
export class EditAllergyComponent {
  submitForm = {
    name: '',
  }

  allergyName: string | null = null;
  errorMessage: string = '';
  showConfirmation: boolean = false;

  constructor(private router: Router, private allergyService: AllergyCatalogService, private route: ActivatedRoute, private messageService: MessageService) { }

  ngOnInit(): void {
    this.allergyName = this.route.snapshot.paramMap.get('allergyName');

    let allergy = null;
    if (this.allergyName != null) {
      allergy = this.allergyService.getAllergyCatalogItem(this.allergyName);
      allergy.subscribe({
        next: (data) => {
          this.submitForm.name = data.name;
        },
        error: (err) => console.error('Error loading allergy', err)
      });
    }
  }

  confirmSubmission(): void {
    this.showConfirmation = true;
  }

  closeConfirmationModal(): void {
    this.showConfirmation = false;
  }

  onSubmit(patientForm: any): void {
    this.showConfirmation = false;
    if (patientForm.valid) {
      const patientData = {
        ...this.submitForm,
      };

      if (this.allergyName) {
        this.allergyService.updateAllergyCatalogItem(
          this.allergyName,
          this.submitForm.name
        ).subscribe(
          (response) => {
            console.log("Allergy edited successfully", response);
            this.messageService.setMessage(`Allergy ${this.allergyName} successfully edited!`);
            this.router.navigate(['/allergiesCatalog']);
          },
          (error) => {
            this.errorMessage = error.error.message;
            console.error("Failed to edit patient", error);
          }
        );
      } else {
        this.errorMessage = 'Medical record number is missing.';
      }
    }
  }

  onCancel(): void {
    this.showConfirmation = false;
    this.router.navigate(['/allergiesCatalog']);
  }
}
