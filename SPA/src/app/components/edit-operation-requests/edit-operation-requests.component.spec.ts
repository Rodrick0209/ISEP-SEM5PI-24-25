import { ComponentFixture, TestBed } from '@angular/core/testing';
import { EditOperationRequestsComponent } from './edit-operation-requests.component';
import { Router } from '@angular/router';
import { ActivatedRoute } from '@angular/router';
import { of } from 'rxjs';
import { OperationRequestService } from '../../services/operationRequestService';
import { MessageService } from '../../services/message.service';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';

describe('EditOperationRequestsComponent', () => {
  let component: EditOperationRequestsComponent;
  let fixture: ComponentFixture<EditOperationRequestsComponent>;
  let mockRouter: any;
  let mockActivatedRoute: any;
  let mockOperationRequestService: any;
  let mockMessageService: any;

  beforeEach(async () => {
    mockRouter = {
      navigate: jasmine.createSpy('navigate')
    };
    mockActivatedRoute = {
      snapshot: {
        paramMap: {
          get: jasmine.createSpy('get').and.returnValue('123')
        }
      }
    };
    mockOperationRequestService = {
      editOperationRequest: jasmine.createSpy('editOperationRequest').and.returnValue(of({}))
    };
    mockMessageService = {
      setMessage: jasmine.createSpy('setMessage')
    };

    await TestBed.configureTestingModule({
      imports: [FormsModule, CommonModule],
      providers: [
        { provide: Router, useValue: mockRouter },
        { provide: ActivatedRoute, useValue: mockActivatedRoute },
        { provide: OperationRequestService, useValue: mockOperationRequestService },
        { provide: MessageService, useValue: mockMessageService }
      ]
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(EditOperationRequestsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should initialize operationRequestId on ngOnInit', () => {
    component.ngOnInit();
    expect(component.operationRequestId).toBe('123');
  });

  it('should show confirmation modal on confirmSubmission', () => {
    component.confirmSubmission();
    expect(component.showConfirmation).toBeTrue();
  });

  it('should close confirmation modal on closeConfirmationModal', () => {
    component.closeConfirmationModal();
    expect(component.showConfirmation).toBeFalse();
  });

  it('should navigate to /operationRequests on successful submission', () => {
    component.operationRequestId = '123';
    component.submitForm.deadline = '2023-12-31';
    component.submitForm.priority = 'High';
    const form = { valid: true };

    component.onSubmit(form);

    expect(mockOperationRequestService.editOperationRequest).toHaveBeenCalled();
    expect(mockMessageService.setMessage).toHaveBeenCalledWith('Operation Request has been updated successfully');
    expect(mockRouter.navigate).toHaveBeenCalledWith(['/operationRequests']);
  });

  it('should set errorMessage if operationRequestId is missing on submission', () => {
    component.operationRequestId = null;
    const form = { valid: true };

    component.onSubmit(form);

    expect(component.errorMessage).toBe('Operation Request ID is missing');
  });

  it('should navigate to /operationRequests on cancel', () => {
    component.onCancel();
    expect(mockRouter.navigate).toHaveBeenCalledWith(['/operationRequests']);
  });
});