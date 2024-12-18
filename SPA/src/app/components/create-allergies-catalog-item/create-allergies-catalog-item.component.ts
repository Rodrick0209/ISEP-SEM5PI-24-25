import { CommonModule } from '@angular/common';
import { Component } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { AllergyCatalogService } from '../../services/allergiesCatalog.service';
import { Router } from '@angular/router';
import { MessageService } from '../../services/message.service';

@Component({
  selector: 'app-create-allergies-catalog-item',
  standalone: true,
  imports : [CommonModule,FormsModule],
  templateUrl: './create-allergies-catalog-item.component.html',
  styleUrl: './create-allergies-catalog-item.component.css'
})
export class CreateAllergiesCatalogItemComponent {
    submitForm = {
      code: '',
      designation: '',
      description: ''
    };

    errorMessage: string = '';
    successMessage: string = '';
    showConfirmation: boolean = false;  

    constructor(private router:Router,private allergiesCatalogService: AllergyCatalogService, private messageService: MessageService) { }

    confirmSubmission(): void {
      this.showConfirmation = true;
    }

    closeConfirmationModal(): void {
      this.showConfirmation = false;
    }

    onSubmit(form: any): void {
      this.showConfirmation = false;
    
      if (form.valid) {    
        this.allergiesCatalogService.createAllergyCatalogItem(this.submitForm.code, this.submitForm.designation, this.submitForm.description).subscribe({
          next: (data: any) => {
            this.messageService.setMessage("Allergy " + data.designation + " successfully created!");
            this.router.navigate(['/allergiesCatalog']);
          },
          error: (err: any) => {
            this.errorMessage = "Failed to create allergy: " + err.error.message;
          }
        });
      }
    }
  
  onCancel(): void {
    this.showConfirmation = false;
    this.router.navigate(['/allergiesCatalog']);
  }
  
}
