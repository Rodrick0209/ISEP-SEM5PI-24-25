import { CommonModule } from '@angular/common';
import { Component } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { AllergyCatalogItem } from '../../models/allergyCatalog';
import { AllergyCatalogService } from '../../services/allergiesCatalog.service';
import { Router } from '@angular/router';
import { MessageService } from '../../services/message.service';

@Component({
  selector: 'app-list-allergies-catalog',
  standalone: true,
  imports: [CommonModule,FormsModule],
  templateUrl: './list-allergies-catalog.component.html',
  styleUrl: './list-allergies-catalog.component.css'
})
export class ListAllergiesCatalogComponent {

    allergiesCatalogItem: AllergyCatalogItem[] = [];
    isLoading = false;  
    message: string = '';
    successMessage: string = '';

    constructor(private allergyCatalogService: AllergyCatalogService, private router: Router, private messageService: MessageService) { }

    ngOnInit(): void {
      this.successMessage = this.messageService.getMessage() ?? '';
      this.message = '';
      this.isLoading = true;
      this.allergyCatalogService.getAllergiesFromCatalog().subscribe({
        next: (data: AllergyCatalogItem[]) => {
          this.isLoading = false;
          this.allergiesCatalogItem = data;
        },
        error: (err: any) => {
          console.error('Failed to fetch allergies', err);
          this.isLoading = false;
          this.allergiesCatalogItem = [];
          this.message = 'An error occurred while fetching allergies: ';
        }
      });

    }


    addAllergy() {
      this.router.navigate(['/allergiesCatalog/add']);
    }

    editAllergy(allergy: AllergyCatalogItem) {
      this.router.navigate(['/allergiesCatalog/edit', allergy.code]);
    }
}
