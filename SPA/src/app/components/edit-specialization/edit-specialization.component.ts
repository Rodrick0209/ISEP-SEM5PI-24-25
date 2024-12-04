import { CommonModule } from '@angular/common';
import { Component } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { SpecializationService } from '../../services/specialization.service';
import { ActivatedRoute, Router } from '@angular/router';

@Component({
  selector: 'app-edit-specialization',
  standalone: true,
  imports: [FormsModule, CommonModule],
  templateUrl: './edit-specialization.component.html',
  styleUrl: './edit-specialization.component.css'
})
export class EditSpecializationComponent {

  oldSpecialization: string | null = null;
  newSpecialization: string = '';
  specializationId: string | null = null;
  message: string = '';
  
  constructor(private router: Router, private specializationService : SpecializationService, private route: ActivatedRoute ) {}

    ngOnInit(): void {
        this.specializationId = this.route.snapshot.paramMap.get('id');
        this.oldSpecialization = this.route.snapshot.paramMap.get('name');
    }

    onSubmit(): void {
      if (this.newSpecialization.length > 0 && this.specializationId) {
        this.specializationService.editSpecialization(this.specializationId, this.newSpecialization).subscribe({
          next: () => {
            this.message = 'Specialization updated successfully';
            this.router.navigate(['/specializations']);
          },
          error: (error: any) => {
            this.message = 'An error occurred while updating the specialization: ' + error.error.message;
          }
        });
      } else {
        this.message = 'Specialization ID is missing or new specialization name is empty';
      }
    }
  
  
    onCancel(): void {
        this.router.navigate(['/specializations']);
    }





}
