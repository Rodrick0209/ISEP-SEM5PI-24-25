import { CommonModule } from '@angular/common';
import { Component } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { AllergyCatalogService } from '../../services/allergiesCatalog.service';
import { Router } from '@angular/router';

@Component({
  selector: 'app-create-allergies-catalog-item',
  standalone: true,
  imports : [CommonModule,FormsModule],
  templateUrl: './create-allergies-catalog-item.component.html',
  styleUrl: './create-allergies-catalog-item.component.css'
})
export class CreateAllergiesCatalogItemComponent {
    allergyToCreate: string = '';
    isLoading = false;
    errorMessage: string = '';
    showConfirmation: boolean = false;

    constructor(private router:Router,private allergiesCatalogService: AllergyCatalogService) { }


    confirmSubmission(): void {
      this.showConfirmation = true;
    }

    closeConfirmationModal(): void {
      this.showConfirmation = false;
    }

    onSubmit(allergyToCreate: any): void {
      this.isLoading = true;
      this.showConfirmation = false;
    
      if (allergyToCreate.valid) {

        const name = this.capitalizeFirstLetter(allergyToCreate.value.name);
    
        this.allergiesCatalogService.createAllergyCatalogItem(name).subscribe({
          next: (data: any) => {
            this.isLoading = false;
            this.router.navigate(['/allergiesCatalog']);
          },
          error: (err: any) => {
            this.isLoading = false;
            this.errorMessage = "Failed to create allergy";
          }
        });
      }
    }

    // Função para capitalizar a primeira letra
  private capitalizeFirstLetter(input: string): string {
    if (!input) return input; // Retorna vazio se a entrada for inválida
    return input.charAt(0).toUpperCase() + input.slice(1).toLowerCase();
  }


  onCancel(): void {
    this.showConfirmation = false;
    this.router.navigate(['/allergiesCatalog']);
  }




}
