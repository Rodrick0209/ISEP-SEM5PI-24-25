import { CommonModule } from '@angular/common';
import { Component, OnInit } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { Router } from '@angular/router';
import { MessageService } from '../../services/message.service';
import { RoomTypeService } from '../../services/room-type.service';

@Component({
  selector: 'app-add-room-type',
  standalone: true,
  imports: [FormsModule, CommonModule],
  templateUrl: './add-room-type.component.html',
  styleUrl: './add-room-type.component.css'
})
export class AddRoomTypeComponent {
  errorMessage: string = '';
  successMessage: string = '';
  submitForm = {
    internalCode: '',
    designation: '',
    description: '',
    suitableForSurgeries: ''
  }

  showConfirmation: boolean = false;

  constructor(private router: Router, private roomTypeService: RoomTypeService, private messageService: MessageService) { }

  confirmSubmission(): void {
    this.showConfirmation = true; // Show confirmation modal
  }

  closeConfirmationModal(): void {
    this.showConfirmation = false; // Close the confirmation modal
  }

  onSubmit(roomTypeForm: any): void {
    this.showConfirmation = false;
    if (roomTypeForm.valid) {
      const roomTypeData = {
        ...this.submitForm,
        suitableForSurgeries: this.submitForm.suitableForSurgeries === 'yes'
      }

      var addRoomType = {
        InternalCode: roomTypeData.internalCode,
        Designation: roomTypeData.designation,
        Description: roomTypeData.description,
        SuitableForSurgeries: roomTypeData.suitableForSurgeries,
      }

      this.roomTypeService.add(addRoomType).subscribe({
        next: (data: any) => {
          console.log('Room Type added successfully', data);
          this.successMessage = `Room Type ${addRoomType.Designation} succesfully added!`;
          this.submitForm = {
            internalCode: '',
            designation: '',
            description: '',
            suitableForSurgeries: ''
          };
        },
        error: (err: any) => {
          console.log('Failed to add room type', err)
          this.errorMessage = 'Failed to add room type: ' + (err.error.message || 'Unknown error')
        }
      });
    } else {
      console.log('Form is invalid');
    }
  }

  onCancel(): void {
    this.showConfirmation = false
    history.back()
  }

}
