import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { PatientService } from '../../services/patient.service';
import { MessageService } from '../../services/message.service';
import { CommonModule } from '@angular/common';
import { MarkXComponent } from '../template/mark-x/mark-x.component';

@Component({
  selector: 'app-delete-patient',
  standalone: true,
  imports: [CommonModule, MarkXComponent],
  templateUrl: './delete-patient.component.html',
  styleUrl: './delete-patient.component.css'
})
export class DeletePatientComponent implements OnInit {
  medicalRecordNumber: string | null = null;
  errorMessage: string = '';

  constructor(private route: ActivatedRoute, private patientService: PatientService, private messageService: MessageService, private router: Router) { }
  
  ngOnInit(): void {
    this.medicalRecordNumber = this.route.snapshot.paramMap.get('medicalRecordNumber');
  }

  isConfirmed = false; // Track if the "X" mark has been clicked


  // Method triggered on delete confirmation
  onDelete(): void {
      if (this.medicalRecordNumber) {
        this.patientService.deletePatient(this.medicalRecordNumber).subscribe({
          next: () => {
            this.messageService.setMessage(`Patient nº ${this.medicalRecordNumber} successfully deleted!`); // Set the success message
            this.router.navigate(['/patients']); // Redirect to the patients list
          },
          error: (err: any) => {
            console.error('Failed to delete patient', err);
            this.errorMessage = 'Failed to delete patient'; // Set the error message
          }
        });
      }
  }

  onCancel(): void {
    this.router.navigate(['/patients']);
  }
}
