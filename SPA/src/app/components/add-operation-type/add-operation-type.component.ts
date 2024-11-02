import { CommonModule } from '@angular/common';
import { Component, EventEmitter, Output } from '@angular/core';
import { FormsModule, NgForm } from '@angular/forms'; // Import FormsModule
import { OperationType, OperationTypesService } from '../../services/operation-type.service';

@Component({
  selector: 'app-add-operation-type',
  standalone: true,
  imports: [FormsModule, CommonModule], // Add FormsModule here
  templateUrl: './add-operation-type.component.html',
  styleUrls: ['./add-operation-type.component.css'] // Correct the styleUrls
})
export class AddOperationTypeComponent {
  @Output() formClosed = new EventEmitter<void>();

  operationType: OperationType = {
    id: this.generateGUID(), // Generate a GUID for ID
    name: '',
    status: '',
    specialization: '',
    preparationPhase: { duration: 0, requiredStaff: [{ num: 0, specialization: '' }] },
    surgeryPhase: { duration: 0, requiredStaff: [{ num: 0, specialization: '' }] },
    cleaningPhase: { duration: 0, requiredStaff: [{ num: 0, specialization: '' }] }
  };
  errorMessage: string | null = null; // Para mensagens de erro


  private generateGUID(): string {
    return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function (c) {
      const r = Math.random() * 16 | 0, v = c === 'x' ? r : (r & 0x3 | 0x8);
      return v.toString(16);
    });
  }

  constructor(private operationTypeService: OperationTypesService) { }

  onSubmit(form: NgForm) {
    this.errorMessage = null; // Resetar a mensagem de erro antes da validação

    if (form.valid) {
      this.operationTypeService.addOperationType(this.operationType).subscribe({
        next: (response) => {
          console.log('Operation Type Added:', response);
          this.formClosed.emit(); // Notify parent component
          this.resetForm();
        },
        error: (error) => {
          console.error('Error adding operation type:', error);
        }
      });
    }else{
      this.errorMessage = 'There was an error submitting the new Operation Type';
    }
  }

  onCancel() {
    console.log('Add operation type cancelled');
    this.resetForm();
    this.formClosed.emit();
  }

  private resetForm() {
    this.operationType = {
      id: '',
      name: '',
      status: '',
      specialization: '',
      preparationPhase: { duration: 0, requiredStaff: [{ num: 0, specialization: '' }] },
      surgeryPhase: { duration: 0, requiredStaff: [{ num: 0, specialization: '' }] },
      cleaningPhase: { duration: 0, requiredStaff: [{ num: 0, specialization: '' }] }
    };
  }

  // Methods for Preparation Phase
  addPreparationStaff() {
    this.operationType.preparationPhase.requiredStaff.push({ num: 0, specialization: '' });
  }

  removePreparationStaff(index: number) {
    this.operationType.preparationPhase.requiredStaff.splice(index, 1);
  }

  // Methods for Surgery Phase
  addSurgeryStaff() {
    this.operationType.surgeryPhase.requiredStaff.push({ num: 0, specialization: '' });
  }

  removeSurgeryStaff(index: number) {
    this.operationType.surgeryPhase.requiredStaff.splice(index, 1);
  }

  // Methods for Cleaning Phase
  addCleaningStaff() {
    this.operationType.cleaningPhase.requiredStaff.push({ num: 0, specialization: '' });
  }

  removeCleaningStaff(index: number) {
    this.operationType.cleaningPhase.requiredStaff.splice(index, 1);
  }


}