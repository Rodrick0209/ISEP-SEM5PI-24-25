import { ComponentFixture, TestBed } from '@angular/core/testing';
import { Router } from '@angular/router';
import { of, throwError } from 'rxjs';
import { CreateOperationRequestsComponent } from './create-operation-requests.component';
import { OperationRequestService } from '../../services/operationRequestService';
import { MessageService } from '../../services/message.service';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';

describe('CreateOperationRequestsComponent', () => {
  let component: CreateOperationRequestsComponent;
  let fixture: ComponentFixture<CreateOperationRequestsComponent>;
  let mockRouter: any;
  let mockOperationRequestService: any;
  let mockMessageService: any;

  beforeEach(async () => {
    mockRouter = {
      navigate: jasmine.createSpy('navigate')
    };
    mockOperationRequestService = {
      createOperationRequest: jasmine.createSpy('createOperationRequest').and.returnValue(of({}))
    };
    mockMessageService = jasmine.createSpyObj(['add']);

    await TestBed.configureTestingModule({
      imports: [CreateOperationRequestsComponent, FormsModule, CommonModule],
      providers: [
        { provide: Router, useValue: mockRouter },
        { provide: OperationRequestService, useValue: mockOperationRequestService },
        { provide: MessageService, useValue: mockMessageService }
      ]
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(CreateOperationRequestsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should show confirmation modal on confirmSubmission', () => {
    component.confirmSubmission();
    expect(component.showConfirmation).toBeTrue();
  });

  it('should close confirmation modal on closeConfirmationModal', () => {
    component.showConfirmation = true;
    component.closeConfirmationModal();
    expect(component.showConfirmation).toBeFalse();
  });

  it('should navigate to /operationRequests on successful submission', () => {
    const form = { valid: true };
    component.onSubmit(form);
    expect(mockOperationRequestService.createOperationRequest).toHaveBeenCalled();
    expect(mockRouter.navigate).toHaveBeenCalledWith(['/operationRequests']);
  });

  it('should set errorMessage on failed submission', () => {
    const form = { valid: true };
    const errorResponse = { error: { message: 'Error occurred' } };
    mockOperationRequestService.createOperationRequest.and.returnValue(throwError(errorResponse));
    component.onSubmit(form);
    expect(component.errorMessage).toBe('An error occurred while creating the operation request: Error occurred');
  });

  it('should navigate to /operationRequests on cancel', () => {
    component.onCancel();
    expect(mockRouter.navigate).toHaveBeenCalledWith(['/operationRequests']);
  });
});