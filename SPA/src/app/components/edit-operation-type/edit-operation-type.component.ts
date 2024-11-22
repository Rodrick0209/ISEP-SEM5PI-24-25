
import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { OperationTypesService, OperationType } from '../../services/operation-type.service';

@Component({
  selector: 'app-edit-operation-type',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './edit-operation-type.component.html',
  styleUrls: ['./edit-operation-type.component.css']
})
export class EditOperationTypeComponent implements OnInit {
  operationType: OperationType = {
    id: '',
    name: '',
    status: 'active', // Default value
    specialization: '',
    preparationPhase: { duration: 0, requiredStaff: [{ num: 0, specialization: '' }] },
    surgeryPhase: { duration: 0, requiredStaff: [{ num: 0, specialization: '' }] },
    cleaningPhase: { duration: 0, requiredStaff: [{ num: 0, specialization: '' }] }
  };
  errorMessage: string | null = null;

  constructor(
    private route: ActivatedRoute,
    private router: Router,
    private operationTypeService: OperationTypesService
  ) {}

  ngOnInit(): void {
    const id = this.route.snapshot.paramMap.get('id');
    if (id) {
      this.loadOperationType(id);
    }
  }

  loadOperationType(id: string): void {
    this.operationTypeService.getOperationTypeById(id).subscribe(
      data => this.operationType = data,
      error => this.errorMessage = 'Failed to load operation type'
    );
  }

  onSubmit(): void {
    this.errorMessage = null;
    this.operationTypeService.updateOperationType(this.operationType).subscribe(
      () => this.router.navigate(['/operationType']),
      error => this.errorMessage = 'Failed to update operation type'
    );
  }

  onCancel(): void {
    this.router.navigate(['/operationType']);
  }

  addPreparationStaff(): void {
    this.operationType.preparationPhase.requiredStaff.push({ num: 0, specialization: '' });
  }

  removePreparationStaff(index: number): void {
    this.operationType.preparationPhase.requiredStaff.splice(index, 1);
  }

  addSurgeryStaff(): void {
    this.operationType.surgeryPhase.requiredStaff.push({ num: 0, specialization: '' });
  }

  removeSurgeryStaff(index: number): void {
    this.operationType.surgeryPhase.requiredStaff.splice(index, 1);
  }

  addCleaningStaff(): void {
    this.operationType.cleaningPhase.requiredStaff.push({ num: 0, specialization: '' });
  }

  removeCleaningStaff(index: number): void {
    this.operationType.cleaningPhase.requiredStaff.splice(index, 1);
  }
}