import { Component } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { SpecializationService } from '../../services/specialization.service';
import { CommonModule } from '@angular/common';


@Component({
  selector: 'app-delete-specialization',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './delete-specialization.component.html',
  styleUrl: './delete-specialization.component.css'
})
export class DeleteSpecializationComponent {

    specializationId: string | null = null;
    errorMessage: string = '';
    isLoading = false;

    constructor(private route: ActivatedRoute, private specializationService: SpecializationService, private router: Router) { }


    ngOnInit(): void {
        this.specializationId = this.route.snapshot.paramMap.get('id');
    }

    isConfirmed = false;


    onMarkClick(): void {
        this.isConfirmed = !this.isConfirmed;
    }

    onDelete(): void {
        this.isLoading = true;
        if (this.specializationId) {
            this.specializationService.deleteSpecialization(this.specializationId).subscribe({
                next: () => {
                    this.isLoading = false;
                    this.router.navigate(['/specializations']);
                },
                error: (err: any) => {
                    this.isLoading = false;
                    console.error('Failed to delete specialization', err);
                }
            });
        }
    }

    onCancel(): void {
        this.router.navigate(['/specializations']);
    }

}
