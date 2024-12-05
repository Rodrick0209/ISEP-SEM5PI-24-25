import { Component } from '@angular/core';
import { Specialization } from '../../models/specialization';
import { CommonModule } from '@angular/common';
import { SpecializationService } from '../../services/specialization.service';
import { MessageService } from '../../services/message.service';
import { Router } from '@angular/router';
import { FormsModule } from '@angular/forms';

@Component({
  selector: 'app-list-specialization',
  standalone: true,
  imports: [CommonModule,FormsModule],
  templateUrl: './list-specialization.component.html',
  styleUrl: './list-specialization.component.css'
})
export class ListSpecializationComponent {
    specializations: Specialization[] = [];
    filterName: string = '';
    errorMessage: string | null = null;
    successMessage: string | null = null;
    isLoading = false;

    constructor(private specializationsService: SpecializationService,private router: Router, private messageService: MessageService) {}
    
    ngOnInit(): void {
      this.isLoading = true;
      this.specializationsService.getSpecializations().subscribe({
        next: (data: Specialization[]) => {
          this.isLoading = false;
          this.specializations = data;
        },
        error: (err: any) => {
          console.error('Failed to fetch specializations', err);
          this.specializations = [];
          this.errorMessage = 'An error occurred while fetching specializations: ' + err.error.message;
        }
    
      });
      this.successMessage = this.messageService.getMessage();
    }




    createSpecialization(): void {
      this.router.navigate(['/specialization/create']);
    }


    filterSpecializations(): void {
      this.errorMessage = null;
      this.specializations = [];
      this.isLoading = true;
      this.specializationsService.getSpecializationsFiltered(this.filterName).subscribe({
        next: (data: Specialization[]) => {
          this.isLoading = false;
          this.specializations = data;
        },
        error: (err: any) => {
          this.isLoading = false;          
          this.specializations = [];
          this.errorMessage = 'Not found any correspondency for the search efected';
        }
    
      });
    
    
    }

    updateSpecialization(id: string, name:string): void {
      this.router.navigate(['/specialization/edit', id,name]);

    }

    deleteSpecialization(id: string): void {
      this.router.navigate(['/specialization/delete', id]);
    }
    




}
